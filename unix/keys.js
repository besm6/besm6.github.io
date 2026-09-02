// KeyboardEvent → {code, mods}, on the main thread (Concept.md §3.5). A
// printable key carries its Unicode codepoint; everything else gets a name
// from the table below. There are no control characters: ^C is 'c' with
// MOD_CTRL, and the kernel decides what that means (§2.3).

export const MOD_SHIFT = 1;
export const MOD_CTRL = 2;
export const MOD_ALT = 4;
export const MOD_META = 8;

// Must match the enum in src/kernel/key.h, in order.
const NAMED = 0x110000;
const NAMES = [
    "Enter", "Backspace", "Tab", "Escape", "Delete", "Insert",
    "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight",
    "Home", "End", "PageUp", "PageDown",
    "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
];

const CODES = new Map(NAMES.map((name, i) => [name, NAMED + i]));

// A named key by name. Text arriving as an input event has no KeyboardEvent to
// normalise, and the key bar has no event at all.
export const named = (name) => CODES.get(name);

// Browser shortcuts the page has no business eating. Ctrl+L is not among them:
// clearing the screen is what it means in a terminal, and the kernel binds it.
// Ctrl+V is, because the paste event it produces is the only way a page may
// read the clipboard without a permission — see `pbpaste`.
const RESERVED = new Set(["r", "R", "t", "T", "w", "W", "n", "N", "v", "V"]);

export function normalise(event) {
    let mods = 0;
    if (event.shiftKey) mods |= MOD_SHIFT;
    if (event.ctrlKey) mods |= MOD_CTRL;
    if (event.altKey) mods |= MOD_ALT;
    if (event.metaKey) mods |= MOD_META;

    const named = CODES.get(event.key);
    if (named !== undefined)
        return { code: named, mods };

    // Alt composes on macOS: Option+Z reports "Ω", so the letter the chord is
    // named for is gone. Take the physical key, which no layout rewrites;
    // elsewhere .key is already the letter and this agrees with it.
    if (event.altKey) {
        const base = /^Key([A-Z])$/.exec(event.code) || /^Digit([0-9])$/.exec(event.code);
        if (base)
            return { code: base[1].toLowerCase().codePointAt(0), mods };
    }

    // A printable key is one whose .key is a single code point; a modifier
    // press on its own, and any dead or composing key, is not one.
    const points = [...event.key];
    if (points.length !== 1)
        return null;

    return { code: points[0].codePointAt(0), mods };
}

// Pasted text as the keystrokes that would have typed it: the terminal has no
// byte stream to write into (§2.3), so a paste is a run of keys and nothing
// downstream can tell it from fast typing. A newline is Enter, however the
// platform spells it; every other control character is dropped, since there is
// no key that produces one. A tab is a space, not Tab: the shell binds Tab to
// completion, and /bin/edit writes a space for one anyway.
export function pasted(text) {
    const codes = [];
    for (const ch of text.replace(/\r\n?/g, "\n")) {
        const code = ch.codePointAt(0);
        if (ch === "\n")
            codes.push(CODES.get("Enter"));
        else if (ch === "\t")
            codes.push(0x20);
        else if (code >= 0x20 && code !== 0x7f)
            codes.push(code);
    }
    return codes;
}

// Whether to preventDefault. Meta is the system's, and a few Ctrl shortcuts
// are the browser's; a page that swallows Ctrl+R is a page you cannot leave.
export function consumes(event, key) {
    if (key === null || event.metaKey)
        return false;
    if (event.ctrlKey && RESERVED.has(event.key))
        return false;
    return true;
}
