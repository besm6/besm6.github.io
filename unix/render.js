// The cell-grid renderer. It reads the kernel's Cell array straight out of
// linear memory and blits monospace glyphs (Concept.md §2.3, §3.5). There is
// no escape-sequence parser because there are no escape sequences.
//
// It draws and nothing else: calling back into wasm from here would re-enter
// the scheduler in the middle of a tick().

// Offsets into the Screen descriptor, in u32s. Mirrors src/kernel/screen.h.
const S_MAGIC = 0;
const S_COLS = 1;
const S_ROWS = 2;
const S_CURSOR_X = 3;
const S_CURSOR_Y = 4;
const S_CURSOR_ON = 5;
const S_CELLS = 6;

const MAGIC = 0x42534352; // 'BSCR'

// One Cell is {u32 ch, u32 fg|bg<<8|attrs<<16}, eight bytes.
const CELL_U32 = 2;

// The kernel clamps every cell it writes (rune_safe, src/kernel/text.h); this
// is the second belt, since fromCodePoint throws and a throw here kills the
// worker that draws the screen.
const glyph = (ch) =>
    String.fromCodePoint(ch > 0x10ffff || (ch >= 0xd800 && ch <= 0xdfff) ? 0xfffd : ch);

const ATTR_BOLD = 1;
const ATTR_UNDERLINE = 2;
const ATTR_REVERSE = 4;

// The palette the kernel's fg/bg indices name: the Linux console's.
const PALETTE = [
    "#111111", "#aa1111", "#11aa11", "#aa5511",
    "#1111aa", "#aa11aa", "#11aaaa", "#aaaaaa",
    "#555555", "#ff5555", "#55ff55", "#ffff55",
    "#5555ff", "#ff55ff", "#55ffff", "#ffffff",
];

const FONT_STACK =
    "'Andale Mono', 'DejaVu Sans Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace";

const FONT_PX = 15;

export class Renderer {
    // `options` is an embedder's: {palette, fontFamily, fontSize}. The defaults
    // above are what index.html gets, and a host page that wants its own theme
    // passes them through braam.js rather than editing this file.
    constructor(canvas, mem, options = {}) {
        this.canvas = canvas;
        this.ctx = canvas.getContext("2d", { alpha: false });
        this.mem = mem;
        this.palette = options.palette || PALETTE;
        this.fontFamily = options.fontFamily || FONT_STACK;
        this.fontSize = options.fontSize || FONT_PX;
        this.info = 0; // descriptor address; 0 until the first resize
        this.fontPx = this.fontSize;
        this.dpr = 1;
        this.cellW = 8;
        this.cellH = 16;
        this.baseline = 12;
        this.sel = null; // {ar, ac, br, bc}: the anchor and the head, in cells
        this.measure();
    }

    // memory.grow detaches the buffer (Concept.md §8.4), so views are derived
    // on every use rather than cached across calls.
    u32() {
        return this.mem.u32();
    }

    // The device-pixel box the page measured. Sizing the backing store here,
    // rather than on the main thread, keeps the font and the geometry together.
    fit(width, height, dpr) {
        this.dpr = dpr;
        this.sel = null; // the cells it named are about to mean something else
        this.canvas.width = Math.max(1, Math.floor(width));
        this.canvas.height = Math.max(1, Math.floor(height));
        this.fontPx = Math.max(8, Math.round(this.fontSize * dpr));
        this.measure();
        return {
            cols: Math.max(1, Math.floor(this.canvas.width / this.cellW)),
            rows: Math.max(1, Math.floor(this.canvas.height / this.cellH)),
        };
    }

    measure() {
        const ctx = this.ctx;
        ctx.font = `${this.fontPx}px ${this.fontFamily}`;
        ctx.textBaseline = "alphabetic";

        const m = ctx.measureText("M");
        // Fractional advances drift over a row, so round once and place every
        // glyph at col * cellW rather than letting the font advance.
        this.cellW = Math.max(1, Math.round(m.width));
        const ascent = m.fontBoundingBoxAscent || this.fontPx * 0.8;
        const descent = m.fontBoundingBoxDescent || this.fontPx * 0.25;
        this.cellH = Math.max(1, Math.round(ascent + descent));
        this.baseline = Math.round(ascent);

        // A proportional font would put every glyph in the wrong place, and the
        // symptom looks like a kernel bug rather than a font one.
        if (Math.abs(ctx.measureText("i").width - m.width) > 0.5)
            console.warn("braam: the terminal font is not monospaced");
    }

    // The descriptor address, learned from resize(). Until it is set there is
    // nothing to draw, which is also the state during boot.
    attach(info) {
        this.info = info;
        this.repaint();
    }

    // Called by the host_present import, once per tick that changed anything.
    present(x, y, w, h) {
        const info = this.info;
        if (!info)
            return;
        const u32 = this.u32();
        const base = info >>> 2;
        if (u32[base + S_MAGIC] !== MAGIC) {
            console.error("braam: screen descriptor magic mismatch");
            return;
        }

        const cols = u32[base + S_COLS];
        const rows = u32[base + S_ROWS];
        const cells = u32[base + S_CELLS] >>> 2;

        const x1 = Math.min(x + w, cols);
        const y1 = Math.min(y + h, rows);
        const cx = Math.min(u32[base + S_CURSOR_X], cols - 1);
        const cy = u32[base + S_CURSOR_Y];
        const cursor = u32[base + S_CURSOR_ON] !== 0;

        const ctx = this.ctx;
        ctx.font = `${this.fontPx}px ${this.fontFamily}`;
        ctx.textBaseline = "alphabetic";

        const sel = this.span();

        for (let row = y; row < y1; row++) {
            const b = this.bounds(sel, row, cols);
            for (let col = x; col < x1; col++) {
                const c = cells + (row * cols + col) * CELL_U32;
                const under = cursor && col === cx && row === cy;
                const marked = b !== null && col >= b[0] && col <= b[1];
                this.cell(col, row, u32[c], u32[c + 1], under !== marked);
            }
        }
    }

    cell(col, row, ch, attr, flip) {
        let fg = attr & 0xff;
        let bg = (attr >>> 8) & 0xff;
        const attrs = (attr >>> 16) & 0xff;

        // The cursor and the selection are drawn, never stored: each reverses
        // the cells it covers, so a cursor inside a selection is a hole in it.
        const reverse = (attrs & ATTR_REVERSE) !== 0;
        if (reverse !== flip)
            [fg, bg] = [bg, fg];

        const px = col * this.cellW;
        const py = row * this.cellH;

        const ctx = this.ctx;
        ctx.fillStyle = this.palette[bg & 0x0f];
        ctx.fillRect(px, py, this.cellW, this.cellH);

        if (ch === 0 || ch === 32)
            return;

        ctx.fillStyle = this.palette[(attrs & ATTR_BOLD ? fg | 8 : fg) & 0x0f];
        ctx.fillText(glyph(ch), px, py + this.baseline);

        if (attrs & ATTR_UNDERLINE)
            ctx.fillRect(px, py + this.cellH - 1, this.cellW, 1);
    }

    // After a resize or a font change there is no damage to go on, so the whole
    // descriptor is repainted.
    repaint() {
        if (!this.info)
            return;
        const u32 = this.u32();
        const base = this.info >>> 2;
        this.present(0, 0, u32[base + S_COLS], u32[base + S_ROWS]);
    }

    // --- Selection. A drag on the page names cells here; the kernel is never
    // told, because a selection is a view over the grid rather than input
    // (Concept.md §3.5).

    // Device pixels to a cell, clamped: a drag that leaves the canvas names an
    // edge cell rather than nothing.
    at(px, py) {
        const u32 = this.u32();
        const base = this.info >>> 2;
        const cols = u32[base + S_COLS];
        const rows = u32[base + S_ROWS];
        return {
            col: Math.min(cols - 1, Math.max(0, Math.floor(px / this.cellW))),
            row: Math.min(rows - 1, Math.max(0, Math.floor(py / this.cellH))),
        };
    }

    // `phase` is "start", "move" or "end". A drag that never left its cell is a
    // click, and a click clears rather than selecting the one cell under it.
    select(phase, px, py) {
        if (!this.info)
            return;
        const was = this.sel;
        const p = this.at(px, py);
        if (phase === "start")
            this.sel = { ar: p.row, ac: p.col, br: p.row, bc: p.col };
        else if (!this.sel)
            return;
        else
            this.sel = { ...this.sel, br: p.row, bc: p.col };

        const s = this.sel;
        if (phase === "end" && s.ar === s.br && s.ac === s.bc)
            this.sel = null;
        this.refresh(was, this.sel);
    }

    // The whole grid, for the select-all chord: what is on the screen and
    // nothing more, which while scrolled back is the view rather than the live
    // screen. The kernel composes that into the cells, so nothing here changes.
    all() {
        if (!this.info)
            return;
        const u32 = this.u32();
        const base = this.info >>> 2;
        const was = this.sel;
        this.sel = { ar: 0, ac: 0, br: u32[base + S_ROWS] - 1, bc: u32[base + S_COLS] - 1 };
        this.refresh(was, this.sel);
    }

    // → true if there was one to drop, so the worker tells the page only when
    // something changed.
    clear() {
        if (!this.sel)
            return false;
        const was = this.sel;
        this.sel = null;
        this.refresh(was, null);
        return true;
    }

    // The anchor and the head in reading order, since a drag may go either way.
    span() {
        const s = this.sel;
        if (!s)
            return null;
        const back = s.br < s.ar || (s.br === s.ar && s.bc < s.ac);
        return back
            ? { r0: s.br, c0: s.bc, r1: s.ar, c1: s.ac }
            : { r0: s.ar, c0: s.ac, r1: s.br, c1: s.bc };
    }

    // The first and last column a selection covers on a row, or null when it
    // covers none of it. Linear, not rectangular: it runs to the end of its
    // first row and starts at the beginning of its last, as a terminal's does,
    // and both ends are inclusive. One rule, so the paint and the text cannot
    // disagree about what is selected.
    bounds(s, row, cols) {
        if (!s || row < s.r0 || row > s.r1)
            return null;
        return [row === s.r0 ? s.c0 : 0, row === s.r1 ? s.c1 : cols - 1];
    }

    // The selected cells as text. Trailing blanks go, so copying a line of a
    // mostly empty screen gives a line rather than a row of spaces.
    text() {
        const s = this.span();
        if (!s)
            return "";
        const u32 = this.u32();
        const base = this.info >>> 2;
        const cols = u32[base + S_COLS];
        const cells = u32[base + S_CELLS] >>> 2;

        const lines = [];
        for (let row = s.r0; row <= s.r1; row++) {
            const [first, last] = this.bounds(s, row, cols);
            let line = "";
            for (let col = first; col <= last; col++) {
                const ch = u32[cells + (row * cols + col) * CELL_U32];
                line += ch === 0 ? " " : glyph(ch);
            }
            lines.push(line.replace(/\s+$/, ""));
        }

        // Trailing blank rows are the grid's padding, not content: a drag past
        // the last line of output, and select-all on a mostly empty screen,
        // must not hand over a screenful of newlines. Blank rows *between*
        // lines are content and stay.
        while (lines.length && lines[lines.length - 1] === "")
            lines.pop();
        return lines.join("\n");
    }

    // Repaint the rows two selections cover between them. A whole-grid repaint
    // per mouse move would do; this is one rectangle through the damage path
    // that already exists.
    refresh(was, now) {
        let lo = Infinity;
        let hi = -Infinity;
        for (const s of [was, now]) {
            if (!s)
                continue;
            lo = Math.min(lo, s.ar, s.br);
            hi = Math.max(hi, s.ar, s.br);
        }
        if (lo > hi)
            return;
        this.present(0, lo, this.u32()[(this.info >>> 2) + S_COLS], hi - lo + 1);
    }
}
