// The kernel lives here. The main thread only relays events and pixels
// (Concept.md §3): OPFS sync handles and OffscreenCanvas both need a worker.

import { makeFsImports, openStore } from "./fs.js";
import { Memory, makeImports } from "./host.js";
import { MOD_SHIFT, named } from "./keys.js";
import { makeProc } from "./proc.js";
import { Renderer } from "./render.js";
import { makeSvcImport } from "./svc.js";

const mem = new Memory();

// One per terminal: its renderer, the sub-row remainder of a wheel gesture, and
// the paste being fed into it. A terminal is made kernel-side by the first
// resize() that names it, so this is filled by whatever the page attaches.
const screens = [];

// resize()'s flags, mirroring src/kernel/screen.h.
const TERM_NO_SHELL = 1;

function pane(term) {
    return screens[term] ||
        (screens[term] = { renderer: null, residue: 0, pasting: null, shell: true });
}

// A canvas or viewport that arrived before the kernel did, by terminal.
const pending = [];

// What an embedder may choose: where the module and the root archive live, and
// how the renderer draws. The defaults are the files beside this one, so a page
// that wants none of this posts nothing (web/braam.js).
let options = {};

// navigator.storage.persist() is main-thread only (Concept.md §A.2), so the
// page calls it and posts the answer down. Boot waits for it rather than
// guessing, since `df` reporting the wrong durability is worse than a tick of
// delay — but only briefly: the page sends a provisional answer when the
// browser is slow to decide, and the real one after it. The second answer
// corrects the store rather than boot.
let persisted = null;
const persistedKnown = new Promise((resolve) => {
    persisted = resolve;
});

let store = null;

// The isolated processes, so that a dispose can let go of their workers.
let proc = null;

// What a keystroke costs. One key is in flight at a time: `key_at` is the stamp
// of the last one that has not been painted yet, and the first present after it
// is the sample.
let key_at = 0;
let repaints = 0;
// The two ends of a measured command, stamped where they happen: a driver
// polling from the page could only bracket them a poll's width apart.
let last_key = 0;
let last_present = 0;
const KEY_SAMPLES = 256;
const keys = [];

// Where a keystroke's time goes, either side of the two boundaries a step adds:
// the kernel's own tick, and the draw into the OffscreenCanvas. Cumulative, so
// a caller takes differences.
const paint = { n: 0, ms: 0 };
const tick = { n: 0, ms: 0 };

// Drawing off, for the A/B that says whether a keystroke's cost is the canvas
// or the turn. The grid is still damaged and still presented; only the pixels
// are skipped, and renderer.text() reads the kernel's memory rather than them,
// so a `selectall` still answers.
let drawing = true;

// The same handshake for the embedder's options, which decide where the module
// is fetched from and therefore cannot be applied after boot has started.
let configured = null;
const configuredKnown = new Promise((resolve) => {
    configured = resolve;
});

function emit(kind, text) {
    self.postMessage({ kind, text });
}

// The boot step under way. The page records it and shows it only if boot stalls.
function stage(text) {
    self.postMessage({ kind: "stage", text });
}

// The clipboard, the file picker and the download need the DOM, so the page
// does them and answers by id. Everything else in svc.js happens right here.
let next_relay = 1;
const relays = new Map();

function relay(msg) {
    return new Promise((resolve, reject) => {
        const id = next_relay++;
        relays.set(id, { resolve, reject });
        self.postMessage({ kind: "svc", id, ...msg });
    });
}

async function boot() {
    const url = new URL(options.wasmUrl || "./kernel.wasm", import.meta.url);
    const rootfs = new URL(options.rootfsUrl || "./rootfs.zip", import.meta.url);
    stage("waiting for the durability answer");
    const durable = await persistedKnown;
    stage(`opening the store from ${rootfs}`);
    store = await openStore(rootfs, durable);

    // A reply arrives on a promise, so it is never on the stack of the tick
    // that issued the request; pumping from here is what gets the resumed task
    // moving when nothing else is scheduled.
    const fs = makeFsImports(mem, store, (token) => {
        self.kernel.wake(token >>> 0, 0, 0);
        pump();
    });

    // Isolated processes live here too (Concept.md §4), each in a worker of this
    // one. It is handed a getter rather than the exports, because the kernel
    // does not exist yet. Nothing has to defer a step: it is a message, and a
    // message is already an event-loop turn, so a process cannot run while the
    // kernel is on the stack.
    proc = makeProc(mem, () => self.kernel,
                    () => new Worker(new URL(options.procWorkerUrl || "./procworker.js",
                                             import.meta.url),
                                     { type: "module" }),
                    () => performance.now());

    // The same rule as storage: a reply must reach the kernel on a promise,
    // never inside the import call that asked for it.
    const svc = makeSvcImport(mem, (slot, obj) => self.kernel.ref(slot >>> 0, obj), relay,
        (token) => {
            self.kernel.wake(token >>> 0, 0, 0);
            pump();
        }, proc);

    const imports = makeImports(mem, (text) => emit("log", text), (term, x, y, w, h) => {
        last_present = performance.now();
        if (key_at) {
            if (keys.length < KEY_SAMPLES)
                keys.push(performance.now() - key_at);
            key_at = 0;
        } else {
            repaints++;
        }
        // A terminal the page has no canvas for is painted nowhere: the kernel
        // keeps its grid either way, and a canvas may arrive later.
        const r = screens[term] && screens[term].renderer;
        if (r && drawing) {
            const at = performance.now();
            r.present(x, y, w, h);
            paint.ms += performance.now() - at;
            paint.n++;
        }
    }, fs, svc);

    // Streaming needs an application/wasm content type; not every static host
    // sets one, so fall back to a buffered instantiate.
    stage(`fetching ${url}`);
    let instance;
    try {
        ({ instance } = await WebAssembly.instantiateStreaming(fetch(url), imports));
    } catch {
        const buf = await (await fetch(url)).arrayBuffer();
        ({ instance } = await WebAssembly.instantiate(buf, imports));
    }

    stage("starting the kernel");
    mem.bind(instance.exports.memory);

    // 0 means "use the linker's __heap_base"; an isolated process (M8) is
    // handed a real base instead.
    instance.exports.init(0);

    self.kernel = instance.exports;

    // The canvas may have arrived before the module finished compiling.
    for (let term = 0; term < pending.length; term++) {
        const held = pending[term];
        if (!held)
            continue;
        pending[term] = null;
        if (held.canvas)
            attach(held.canvas, term);
        if (held.viewport)
            fit(held.viewport, term);
    }
    pump();
}

// A screen may name a palette or a font of its own; the rest is the mount's.
function attach(msg, term) {
    const own = {};
    for (const key of ["palette", "fontFamily", "fontSize"])
        if (msg[key] !== undefined)
            own[key] = msg[key];
    const p = pane(term);
    // Read by the first fit(), which is what makes the terminal kernel-side.
    if (msg.shell === false)
        p.shell = false;
    p.renderer = new Renderer(msg.canvas, mem, { ...options, ...own });
}

// A mouse selection is the page's gesture and the renderer's highlight, and the
// kernel is told nothing about either (Concept.md §3.5). The text crosses back
// when it settles, because only the page can reach the clipboard.
function deselect(term) {
    const r = screens[term] && screens[term].renderer;
    if (r && r.clear())
        self.postMessage({ kind: "selection", term, text: "" });
}

// The wheel is the page's gesture too, and reaches the kernel as the keystrokes
// the scrollback chord is made of (Concept.md §3.5) — so there is no mouse in
// the ABI here either. The worker owns the font, so this is the side that can
// turn device pixels into rows; the fraction of a row left over is carried, or
// a trackpad's small deltas would never scroll at all.
const KEY_UP = named("ArrowUp");
const KEY_DOWN = named("ArrowDown");
const KEY_PAGE_UP = named("PageUp");
const KEY_PAGE_DOWN = named("PageDown");

// One flick may not outrun the key ring, which holds 64.
const WHEEL_MAX = 64;

function scroll({ dy, mode, term }) {
    const p = pane(term);
    if (!p.renderer)
        return;
    let rows = dy;
    const page = mode === 2; // pages: the chord's own half a screen each
    if (mode !== 1 && !page) {
        if ((dy < 0) !== (p.residue < 0))
            p.residue = 0; // a reversal continues nothing
        const exact = (p.residue + dy) / p.renderer.cellH;
        rows = Math.trunc(exact);
        p.residue = (exact - rows) * p.renderer.cellH;
    }
    const n = Math.trunc(rows);
    if (!n)
        return;

    const back = n < 0;
    const code = page ? (back ? KEY_PAGE_UP : KEY_PAGE_DOWN) : (back ? KEY_UP : KEY_DOWN);
    deselect(term);
    // Fed like a paste: a full ring is back-pressure, and the rest of the flick
    // is dropped rather than queued. One pump, so the whole run costs one paint.
    for (let i = Math.min(Math.abs(n), WHEEL_MAX); i > 0; i--)
        if (!self.kernel.key(term, code, MOD_SHIFT))
            break;
    pump();
}

// The worker owns the font, so it owns the geometry: the page reports a box in
// device pixels and reads back whatever the kernel accepted.
function fit({ width, height, dpr }, term) {
    const p = pane(term);
    if (!p.renderer)
        return;
    deselect(term);
    p.residue = 0; // the row height it is a fraction of is about to change
    const { cols, rows } = p.renderer.fit(width, height, dpr);
    const info = self.kernel.resize(term, cols, rows, p.shell ? 0 : TERM_NO_SHELL);
    if (info === 0) {
        emit("error", `braam: no memory for a ${cols}x${rows} screen`);
        return;
    }
    p.renderer.attach(info);
}

// The event loop is the scheduler (Concept.md §2.1). tick() drains the ready
// queue and says how long until it next needs to run; -1 means idle.
let timer = null;

function pump() {
    if (timer !== null) {
        clearTimeout(timer);
        timer = null;
    }
    const at = performance.now();
    const delay = self.kernel.tick(at);
    tick.ms += performance.now() - at;
    tick.n++;
    if (delay >= 0)
        timer = setTimeout(pump, delay);
}

// A paste is a run of keystrokes (web/keys.js), and a run is longer than the
// key ring — so it is fed at the rate the console drains it rather than all at
// once, which would drop everything past the ring's last slot. key() says
// whether it took the keystroke; a refusal means the ring is full, and the rest
// of the run waits for the tick that empties it. A second paste joins the queue
// instead of displacing it; a key typed while one is being fed goes straight in
// ahead of the rest, since ^C must not wait behind a paste.
// pane().pasting is { codes, at }, the run being fed into that terminal.
function feed(term) {
    const p = pane(term);
    const run = p.pasting;
    while (run.at < run.codes.length && self.kernel.key(term, run.codes[run.at] >>> 0, 0))
        run.at++;
    pump();
    if (run.at < run.codes.length)
        setTimeout(() => feed(term), 0);
    else
        p.pasting = null;
}

function type(codes, term) {
    const p = pane(term);
    if (p.pasting) {
        p.pasting.codes = p.pasting.codes.slice(p.pasting.at).concat(codes);
        p.pasting.at = 0;
        return; // the feed already waiting on a turn will take it
    }
    p.pasting = { codes, at: 0 };
    feed(term);
}

// Events reach a suspended task as a wake token, never as a return value
// (Concept.md §2.2). Every one of them pumps: when nothing is sleeping there is
// no timer armed, so queued work would otherwise sit there forever.
self.onmessage = ({ data }) => {
    if (!data)
        return;

    // Boot itself waits on these two, so they are answered before the kernel
    // exists. `options` must arrive first of all, since boot reads it.
    if (data.kind === "options") {
        options = data.options || {};
        configured(true);
        return;
    }

    if (data.kind === "persisted") {
        persisted(!!data.value);
        if (store)
            store.persisted = !!data.value;
        return;
    }

    // The page is letting go, and answered before boot has finished as well as
    // after: a process is a worker of this one, and one spinning in a
    // loop is a core burning until somebody says stop.
    if (data.kind === "dispose") {
        if (proc)
            proc.shutdown();
        self.close();
        return;
    }

    // What the measurement read (doc/Release_Notes.md). Answered before the kernel
    // exists too, so a driver can poll from the moment it mounts, and it draws
    // nothing: a `selectall` poll would repaint the grid it is timing.
    if (data.kind === "stats") {
        self.postMessage({
            kind: "stats",
            now: performance.now(),
            proc: proc ? proc.stats() : null,
            keys: keys.slice(),
            repaints,
            last_key,
            last_present,
            paint: { ...paint },
            tick: { ...tick },
            drawing,
        });
        // Only the key samples, which a caller indexes by position. The
        // counters stay cumulative so a caller can take differences.
        if (data.reset) {
            keys.length = 0;
            key_at = 0;
        }
        return;
    }

    // The other half of the measurement: the same run with nothing drawn.
    if (data.kind === "render") {
        drawing = !!data.on;
        return;
    }

    if (data.kind === "svc-reply") {
        const waiting = relays.get(data.id);
        if (!waiting)
            return;
        relays.delete(data.id);
        if (data.error)
            waiting.reject({ braam: data.error });
        else
            waiting.resolve(data.value);
        return;
    }

    // Which terminal the page means. Absent is 0, so a single-screen embedder
    // posts what it always did (web/braam.js).
    const term = data.term >>> 0;

    if (!self.kernel) {
        const held = pending[term] || (pending[term] = {});
        if (data.kind === "canvas")
            held.canvas = data;
        else if (data.kind === "viewport")
            held.viewport = data;
        return;
    }

    const r = screens[term] && screens[term].renderer;

    switch (data.kind) {
    case "canvas":
        attach(data, term);
        break;
    case "viewport":
        fit(data, term);
        pump();
        break;
    case "key":
        deselect(term);
        key_at = performance.now();
        last_key = key_at;
        self.kernel.key(term, data.code >>> 0, data.mods >>> 0);
        pump();
        break;
    case "scroll":
        scroll(data);
        break;
    case "select":
        if (r) {
            r.select(data.phase, data.x, data.y);
            if (data.phase === "end")
                self.postMessage({ kind: "selection", term, text: r.text() });
        }
        break;
    case "selectall":
        // Answered even with nothing to answer with, and carrying back the id
        // the asker sent: a driver cannot tell a blank screen from a dropped
        // request otherwise, and deselect() posts a selection nobody asked for.
        if (r)
            r.all();
        self.postMessage({
            kind: "selection",
            term,
            text: r ? r.text() : "",
            id: data.id,
        });
        break;
    case "paste":
        deselect(term);
        type(data.codes || [], term);
        break;
    case "deselect":
        deselect(term);
        break;
    case "wake":
        self.kernel.wake(data.token >>> 0, data.ptr >>> 0, data.len >>> 0);
        pump();
        break;
    }
};

// Nothing is fetched until the embedder's options arrive, since they say what
// to fetch. web/braam.js posts them before anything else.
stage("waiting for the embedder's options");
configuredKnown
    .then(boot)
    .catch((e) => emit("error", `boot failed: ${e && e.message ? e.message : e}`));
