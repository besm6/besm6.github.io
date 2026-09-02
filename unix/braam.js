// The embedding API: everything a host page needs to put a braam terminal on
// itself, and nothing about how braam works.
//
//   import { mount } from "./braam.js";
//   const term = mount({ canvas: document.getElementById("screen") });
//
// One instance is one worker, which is the isolation M8 builds on rather than
// an accident, so mounting twice on a page gives two independent kernels that
// share nothing but the origin's storage. Two *screens* of one kernel are the
// other arrangement — `mount({screens: [...]})`, one shell and one console per
// screen over one scheduler and one filesystem (web/dual.html).
//
// Everything that used to be inline in index.html lives here: the worker, the
// OffscreenCanvas transfer, the resize and device-pixel-ratio watches, the
// keyboard, and the three services a worker cannot perform for itself.
//
// What the page styles, since this file styles nothing the page owns:
// `touch-action: none` and `-webkit-touch-callout: none` on the canvas, the
// focus ring as `canvas.braam-focus`, and `.braam-key` for the buttons in the
// `keys` container. Sizing to `visualViewport` is the page's too. See
// index.html.
//
// A right-click raises the browser's own text menu, over the hidden input;
// `mount({menu: false})` declines that and leaves the canvas its own.

import { E } from "./abi.js";
import { MOD_CTRL, consumes, named, normalise, pasted } from "./keys.js";

// Persistence is granted to the origin, not to a terminal, so the request is
// made once for the page however many are mounted. Asking twice is not merely
// redundant: a second call while the first is outstanding may not settle until
// the first one has, and a worker whose boot waits on the answer waits with it.
let persistence = null;

function persistOnce(wanted) {
    if (!wanted)
        return Promise.resolve(false);
    if (!persistence) {
        persistence = (async () => {
            try {
                if (navigator.storage && navigator.storage.persist)
                    return await navigator.storage.persist();
            } catch {
                // A refusal is an answer; `df` reports best-effort mode.
            }
            return false;
        })();
    }
    return persistence;
}

// How long boot may wait for that answer. M6 blocked on it outright, on the
// reasoning that `df` reporting the wrong durability is worse than a tick of
// delay — and it is, but the call took over five seconds in headless Firefox,
// which is a blank screen rather than a tick. So a provisional "best effort"
// goes down after this, and the real answer follows and corrects it.
const PERSIST_GRACE_MS = 250;

// How long a boot may take before it is reported as stuck. A stalled fetch
// throws nothing and leaves a black canvas.
const BOOT_STALL_MS = 5000;

// One screen: the canvas, the hidden input behind it, the pointer and keyboard
// listeners, and the key bar. Everything here names its terminal, and nothing
// here knows there is more than one.
//
// `session` is what the panes of one mount share — the worker they post to, the
// clipboard wait a `pbpaste` on any screen parks on, and the file input.
function makePane(session, spec, term) {
    const { worker, onError } = session;
    const canvas = spec.canvas;

    const offscreen = canvas.transferControlToOffscreen();
    // A screen may draw in colours of its own, which is how two panes on one
    // page are told apart; anything it does not name is the mount's. `shell:
    // false` leaves the terminal bare, for a program that opens it instead.
    worker.postMessage({
        kind: "canvas",
        term,
        canvas: offscreen,
        palette: spec.palette,
        fontFamily: spec.fontFamily,
        fontSize: spec.fontSize,
        shell: spec.shell,
    }, [offscreen]);

    // The page owns the pixel box; the worker owns the font and therefore the
    // geometry. devicePixelContentBoxSize is the exact device-pixel box, and is
    // already right under fractional zoom.
    function viewport(entry) {
        const dpr = window.devicePixelRatio || 1;
        let width, height;
        if (entry && entry.devicePixelContentBoxSize && entry.devicePixelContentBoxSize.length) {
            const box = entry.devicePixelContentBoxSize[0];
            width = box.inlineSize;
            height = box.blockSize;
        } else {
            const rect = canvas.getBoundingClientRect();
            width = Math.round(rect.width * dpr);
            height = Math.round(rect.height * dpr);
        }
        worker.postMessage({ kind: "viewport", term, width, height, dpr });
    }

    const observer = new ResizeObserver((entries) => viewport(entries[0]));
    observer.observe(canvas);

    // devicePixelRatio changes when the window moves to another monitor or the
    // browser zooms; the query has to be re-armed at the new ratio each time.
    function watchRatio() {
        matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`).addEventListener(
            "change",
            () => {
                if (!session.live)
                    return;
                viewport(null);
                watchRatio();
            },
            { once: true });
    }
    watchRatio();

    // The focus target: a hidden textarea, because a software keyboard is
    // raised by a focused editable element and a canvas is not one
    // (Concept.md §3.5). Every declaration below is load-bearing — see
    // Release_Notes.md before changing one.
    //
    // It holds a sentinel and, behind it, the grid's selection: that is what
    // the browser's Edit menu acts on (§3.5). A no-break space, not a
    // zero-width one, so a composing input method cannot absorb it.
    const SENTINEL = "\u00a0";
    const sink = document.createElement("textarea");
    sink.setAttribute("aria-label", "Terminal input");
    sink.setAttribute("autocapitalize", "none");
    sink.setAttribute("autocorrect", "off");
    sink.setAttribute("autocomplete", "off");
    sink.setAttribute("spellcheck", "false");
    sink.setAttribute("enterkeyhint", "enter");
    sink.setAttribute("inputmode", "text"); // never "none" — that hides the keyboard
    sink.setAttribute("translate", "no");
    sink.rows = 1;
    sink.wrap = "off";
    sink.style.cssText =
        "position:fixed;left:0;top:0;width:1px;height:1px;margin:0;padding:0;border:0;" +
        "outline:none;resize:none;opacity:0;pointer-events:none;z-index:-1;overflow:hidden;" +
        "white-space:pre;color:transparent;background:transparent;caret-color:transparent;" +
        "font-size:16px";
    (document.body || document.documentElement).appendChild(sink);

    function focusSink() {
        sink.focus({ preventScroll: true });
    }

    // The focus back, after a turn in which the engine may have dropped it —
    // cancelling a composition blurs on some. Taken back, never taken: a sink
    // that still holds it is left alone.
    function keepFocus() {
        if (document.activeElement !== sink)
            focusSink();
    }

    // The right-click menu is the browser's own, and it acts on what the
    // pointer is over — so the sink covers the canvas for the length of a
    // secondary press (§3.5). The timer restores it when no menu follows.
    const MENU_GRACE_MS = 1500;

    let armed = 0; // the restore timer, or 0

    function armSink() {
        const rect = canvas.getBoundingClientRect();
        sink.style.left = `${rect.left}px`;
        sink.style.top = `${rect.top}px`;
        sink.style.width = `${rect.width}px`;
        sink.style.height = `${rect.height}px`;
        sink.style.pointerEvents = "auto";
        sink.style.zIndex = "2147483647";
        clearTimeout(armed);
        armed = setTimeout(restSink, MENU_GRACE_MS);
    }

    function restSink() {
        clearTimeout(armed);
        armed = 0;
        sink.style.left = "0px";
        sink.style.top = "0px";
        sink.style.width = "1px";
        sink.style.height = "1px";
        sink.style.pointerEvents = "none";
        sink.style.zIndex = "-1";
        resetSink(); // whatever the press selected, the mirror is the range
    }

    // A secondary press that moved the caret would collapse the range the
    // menu's Copy reads; any other press on an armed sink is the canvas's.
    function onSinkMouseDown(event) {
        if (event.button === 2 || event.ctrlKey)
            event.preventDefault();
        else if (armed)
            restSink();
    }

    // The menu is built after this returns, so the range goes back here for an
    // engine that moved the caret anyway — and again on the next turn, for one
    // that takes the word under the press after dispatching this.
    function onSinkContextMenu() {
        if (!composing)
            sink.setSelectionRange(SENTINEL.length, sink.value.length);
        setTimeout(restSink, 0);
    }

    const wantsMenu = session.options.menu !== false;

    if (wantsMenu) {
        sink.addEventListener("mousedown", onSinkMouseDown);
        sink.addEventListener("contextmenu", onSinkContextMenu);
    }

    function onSinkFocus() {
        canvas.classList.add("braam-focus");
    }

    function onSinkBlur() {
        canvas.classList.remove("braam-focus");
        setSticky(0);
    }

    sink.addEventListener("focus", onSinkFocus);
    sink.addEventListener("blur", onSinkBlur);

    // A page that focuses the canvas itself must not land on an element that
    // reads no keys.
    canvas.addEventListener("focus", focusSink);

    // iOS raises the keyboard only from a trusted gesture, and click is the one
    // every version honours. It does not replace the pointerdown below.
    canvas.addEventListener("click", focusSink);

    // The mouse selects, and nothing about it reaches the kernel: the page
    // names cells in device pixels, the renderer highlights them and reads them
    // back as text (Concept.md §3.5). It costs no keystroke and no syscall.
    let selection = "";
    let dragging = null; // the pointer id of the drag in progress

    // An input method is mid-word; nothing may touch the sink until it is done.
    let composing = false;

    // The sink's value is the sentinel and the selection behind it, with the
    // range over the selection alone — so the resting range never starts at 0,
    // a browser Select All always changes it, and a browser Copy has the right
    // text under it (Concept.md §3.5).
    function resetSink() {
        if (composing)
            return;
        sink.value = SENTINEL + selection;
        sink.setSelectionRange(SENTINEL.length, sink.value.length);
    }

    // A composition the engine opened and never closed: Chrome cancels one on
    // Esc without firing compositionend, so the flag stays up, every keydown
    // after it still reports isComposing, and both key routes are shut for
    // good. Esc ends one whatever the engine says.
    function endComposing() {
        if (!composing)
            return;
        composing = false;
        resetSink();
    }

    // Any input drops the selection, here as in the worker.
    function dropSelection() {
        if (!selection)
            return;
        selection = "";
        resetSink();
    }

    resetSink();

    // Select All off the browser's Edit menu: the one range that reaches column
    // 0. Collapsing it back rejects the duplicate an engine firing more than
    // one of these sends; the worker's reply installs the mirror.
    function onSelectAll() {
        // A secondary press selects the word under it, which in a one-line
        // sink is the whole of it; only a range from outside the press is the
        // command.
        if (armed || document.activeElement !== sink || !sink.value.length)
            return;
        if (sink.selectionStart !== 0 || sink.selectionEnd !== sink.value.length)
            return;
        sink.setSelectionRange(SENTINEL.length, sink.value.length);
        worker.postMessage({ kind: "selectall", term });
    }

    // Which of the three an engine fires for a text control differs; the guard
    // above makes taking all of them safe.
    sink.addEventListener("select", onSelectAll);
    sink.addEventListener("selectionchange", onSelectAll);
    document.addEventListener("selectionchange", onSelectAll);

    function device(event) {
        const rect = canvas.getBoundingClientRect();
        const dpr = window.devicePixelRatio || 1;
        return {
            x: Math.round((event.clientX - rect.left) * dpr),
            y: Math.round((event.clientY - rect.top) * dpr),
        };
    }

    function drag(phase, event) {
        const { x, y } = device(event);
        worker.postMessage({ kind: "select", term, phase, x, y });
    }

    function onPointerDown(event) {
        // A second finger is not a second drag.
        if (!event.isPrimary)
            return;
        // A click has to focus, or the copy chord below would go to whatever
        // the page focused last.
        focusSink();
        // The secondary button, which Ctrl+click is on a Mac: a menu rather
        // than a drag, and the sink has to be under it before contextmenu.
        if (event.button === 2 || event.ctrlKey) {
            if (wantsMenu)
                armSink();
            return;
        }
        if (event.button !== 0)
            return;
        dragging = event.pointerId;
        canvas.setPointerCapture(event.pointerId);
        drag("start", event);
    }

    function onPointerMove(event) {
        if (dragging === event.pointerId)
            drag("move", event);
    }

    function onPointerUp(event) {
        if (dragging !== event.pointerId)
            return;
        dragging = null;
        drag("end", event);
    }

    canvas.addEventListener("pointerdown", onPointerDown);
    canvas.addEventListener("pointermove", onPointerMove);
    canvas.addEventListener("pointerup", onPointerUp);
    canvas.addEventListener("pointercancel", onPointerUp);

    // The wheel scrolls the screen's history, and reaches the kernel as the
    // keystrokes that chord is (Concept.md §3.5): there is no mouse event in
    // the ABI, here as for the selection. The delta crosses in device pixels
    // because the worker owns the font and therefore the row height; a line or
    // page delta is a count and goes unscaled.
    function onWheel(event) {
        // The browser's zoom, which a trackpad pinch also arrives as.
        if (event.ctrlKey || event.metaKey)
            return;
        event.preventDefault(); // the canvas owns the wheel over itself
        if (!event.deltaY)
            return;
        const dpr = window.devicePixelRatio || 1;
        worker.postMessage({
            kind: "scroll",
            term,
            dy: event.deltaMode === 0 ? event.deltaY * dpr : event.deltaY,
            mode: event.deltaMode,
        });
    }

    canvas.addEventListener("wheel", onWheel, { passive: false });

    // What both copy routes owe the selection: ^C interrupts again from here,
    // without waiting for a reply.
    function copied() {
        selection = "";
        resetSink();
        worker.postMessage({ kind: "deselect", term });
    }

    function copy() {
        const text = selection;
        copied();
        if (!navigator.clipboard) {
            onError("braam: this origin has no clipboard");
            return;
        }
        // Inside the keydown on purpose: the keystroke is the transient
        // activation that permits the write (Concept.md §A.2). The page's own
        // copy of the text is what makes that possible — asking the worker for
        // it would answer a turn too late.
        navigator.clipboard.writeText(text)
            .catch((e) => onError(`braam: copy refused: ${e.message}`));
    }

    // Copy and Cut off the browser's Edit menu, which the chord above never
    // reaches: it prevents its own default, so no copy event follows it. Cut
    // shares this — a terminal has nothing to cut, and the native one would
    // take the mirror out of the sink. The event is the document's, so a
    // terminal claims one only while it holds the focus.
    function onCopy(event) {
        if (document.activeElement !== sink || !selection || !event.clipboardData)
            return;
        event.preventDefault();
        event.clipboardData.setData("text/plain", selection);
        copied();
    }

    addEventListener("copy", onCopy);
    addEventListener("cut", onCopy);

    // Cmd+V, or Ctrl+V where that is the chord: the browser hands the text over
    // and the terminal types it. A `pbpaste` that is waiting takes it instead —
    // it asked for exactly this gesture, and a program reading the clipboard
    // wants the text rather than the keystrokes.
    //
    // The paste event is the document's, not the canvas's, so a pane only
    // claims one when it holds the focus; otherwise the paste belongs to
    // whatever field the page focused.
    function onPaste(event) {
        if (document.activeElement !== sink)
            return;
        const text = event.clipboardData ? event.clipboardData.getData("text") : "";
        if (session.pasteWaiter) {
            const resolve = session.pasteWaiter;
            session.pasteWaiter = null;
            event.preventDefault();
            resolve(text);
            return;
        }
        if (!text)
            return;
        event.preventDefault();
        worker.postMessage({ kind: "paste", term, codes: pasted(text) });
    }
    addEventListener("paste", onPaste);

    const letter = (event, c) =>
        !event.altKey && (event.key === c || event.key === c.toUpperCase());

    // A modifier latched on the key bar, applying to the next key and to
    // nothing after it. One bitmask, so Shift or Alt could join MOD_CTRL.
    let sticky = 0;
    let ctrlButton = null; // the Ctrl button, if there is a bar

    function setSticky(mods) {
        sticky = mods;
        if (ctrlButton) {
            const on = (sticky & MOD_CTRL) !== 0;
            ctrlButton.classList.toggle("braam-key-on", on);
            ctrlButton.setAttribute("aria-pressed", on ? "true" : "false");
        }
    }

    // The one way a keystroke leaves for the kernel.
    function sendKey(code, mods) {
        dropSelection();
        worker.postMessage({ kind: "key", term, code, mods: mods | sticky });
        setSticky(0);
    }

    // Everything the input path below produces goes as a paste run instead: it
    // is paced against the key ring, and worker.js dispatches a key ahead of a
    // run still being fed, which would reorder a backspace against its word.
    function typeCodes(codes) {
        dropSelection();
        if (codes.length)
            worker.postMessage({ kind: "paste", term, codes });
    }

    // Scoped to the sink rather than the window: a terminal shares its page,
    // and two of them must not both read the same keystroke.
    function onKeyDown(event) {
        // Esc ends a composition (endComposing), and the engine that cancels
        // one may blur with it. The focus is checked a turn later, since the
        // blur follows this handler.
        if (event.key === "Escape") {
            endComposing();
            setTimeout(keepFocus, 0);
        }

        // A soft keyboard reports a key it has not decided on yet — GBoard
        // sends keyCode 229 with key "Unidentified". Those arrive as input
        // events instead. What is open is what the composition events said,
        // not event.isComposing — an engine leaves that set on every key
        // after a cancelled composition.
        if (composing || event.keyCode === 229 || event.key === "Unidentified")
            return;

        // Ctrl+C — Cmd+C on a Mac — copies when there is a selection, and is
        // ^C when there is not. A terminal with no second copy key has to
        // overload it, and copying clears the selection, so the next one
        // interrupts. Every other key just drops the selection, in the worker.
        if (selection && (event.ctrlKey || event.metaKey) && letter(event, "c")) {
            event.preventDefault(); // or the browser copies its empty selection over ours
            copy();
            return;
        }

        // Select all is the platform's chord and not Ctrl+A, which is the line
        // editor's beginning-of-line and cannot be overloaded: there is no
        // "there is a selection" to tell the two apart the way there is for
        // copy. A browser that claims Ctrl+Shift+A for itself keeps it.
        if ((event.metaKey || (event.ctrlKey && event.shiftKey)) && letter(event, "a")) {
            event.preventDefault();
            worker.postMessage({ kind: "selectall", term });
            return;
        }

        const key = normalise(event);
        if (consumes(event, key))
            event.preventDefault();
        if (key)
            sendKey(key.code, key.mods);
    }

    // The other source of keys, for a keyboard that reports none: dictation,
    // predictive text and every IME. It runs exactly when onKeyDown did not
    // prevent the default, which is what keeps a keystroke from arriving twice.
    //
    // What follows the sentinel is whatever the input method has just produced:
    // an insertion replaces the mirror, which is what the range covers. Reading
    // the value rather than event.data makes the order of input and
    // compositionend, which differs between engines, not matter.

    function drain() {
        const raw = sink.value;
        const text = raw.startsWith(SENTINEL) ? raw.slice(SENTINEL.length) : raw;
        dropSelection();
        resetSink();
        if (!text)
            return;
        let codes = pasted(text);
        // Ctrl latched on the bar, then "c" on the soft keyboard, is ^C. It
        // goes as a key precisely because that jumps the paste queue.
        if (sticky && codes.length) {
            sendKey(codes[0], 0);
            codes = codes.slice(1);
        }
        typeCodes(codes);
    }

    // A delete against a field the sentinel keeps non-empty is a real edit, but
    // it is still taken here, where it is announced before the fact.
    const DELETES = { deleteContentBackward: "Backspace", deleteContentForward: "Delete" };

    function onBeforeInput(event) {
        const name = DELETES[event.inputType];
        if (name)
            typeCodes([named(name)]);
    }

    // A paste is the paste event's; history and drag insertions have no
    // keystroke that would have produced them.
    const IGNORED = /^(delete|history|insertFromPaste|insertFromDrop)/;

    function onInput(event) {
        if (composing)
            return; // still being edited; compositionend drains it
        if (event.inputType === "insertLineBreak" || event.inputType === "insertParagraph") {
            typeCodes([named("Enter")]);
            resetSink();
            return;
        }
        if (IGNORED.test(event.inputType || "")) {
            resetSink();
            return;
        }
        drain();
    }

    function onCompositionStart() {
        composing = true;
    }

    function onCompositionEnd() {
        composing = false;
        drain();
    }

    sink.addEventListener("keydown", onKeyDown);
    sink.addEventListener("beforeinput", onBeforeInput);
    sink.addEventListener("input", onInput);
    sink.addEventListener("compositionstart", onCompositionStart);
    sink.addEventListener("compositionend", onCompositionEnd);

    // Esc, Tab, Ctrl and the arrows: the keys a software keyboard does not
    // have. The page places the container and styles it; only the behaviour is
    // here. Nothing about it reaches the kernel — a tapped Esc and a typed one
    // are the same {code, mods} (Concept.md §3.5).
    //
    // A bar belongs to the screen whose spec named it: every button closes over
    // that pane's sendKey, so two screens take two bars.
    const barKeys = [];

    function makeKey(container, label, code, mods) {
        const button = document.createElement("button");
        button.type = "button";
        button.className = "braam-key";
        button.textContent = label;
        if (mods)
            button.setAttribute("aria-pressed", "false");
        // Preventing mousedown's default is what stops the focus leaving the
        // sink, which would dismiss the keyboard the bar exists to complement.
        button.addEventListener("mousedown", (event) => event.preventDefault());
        button.addEventListener("click", (event) => {
            event.preventDefault();
            if (mods)
                setSticky(sticky ^ mods);
            else
                sendKey(code, 0);
            focusSink();
        });
        container.appendChild(button);
        barKeys.push(button);
        return button;
    }

    if (spec.keys) {
        const bar = spec.keys;
        makeKey(bar, "Esc", named("Escape"), 0);
        makeKey(bar, "Tab", named("Tab"), 0);
        ctrlButton = makeKey(bar, "Ctrl", 0, MOD_CTRL);
        makeKey(bar, "←", named("ArrowLeft"), 0);
        makeKey(bar, "↑", named("ArrowUp"), 0);
        makeKey(bar, "↓", named("ArrowDown"), 0);
        makeKey(bar, "→", named("ArrowRight"), 0);
    }

    return {
        canvas,
        term,

        focus() {
            focusSink();
        },

        focused() {
            return document.activeElement === sink;
        },

        // What the mouse has marked, which is what the copy chord writes.
        selection() {
            return selection;
        },

        // The worker's answer to a drag or a select-all, which the menu's Copy
        // acts on — so the mirror follows the grid.
        deliver(text) {
            selection = text;
            resetSink();
        },

        dispose() {
            clearTimeout(armed);
            observer.disconnect();
            canvas.removeEventListener("focus", focusSink);
            canvas.removeEventListener("click", focusSink);
            canvas.removeEventListener("pointerdown", onPointerDown);
            canvas.removeEventListener("pointermove", onPointerMove);
            canvas.removeEventListener("pointerup", onPointerUp);
            canvas.removeEventListener("pointercancel", onPointerUp);
            canvas.removeEventListener("wheel", onWheel);
            canvas.classList.remove("braam-focus");
            removeEventListener("paste", onPaste);
            removeEventListener("copy", onCopy);
            removeEventListener("cut", onCopy);
            document.removeEventListener("selectionchange", onSelectAll);
            // The sink and the buttons go with their listeners; the container
            // is the page's and stays.
            sink.remove();
            for (const button of barKeys)
                button.remove();
        },
    };
}

// A canvas hands its pixels over exactly once, so a terminal that has been
// disposed cannot be mounted again on the same element — make a new one.
export function mount(options = {}) {
    // One canvas or several. `screens` is the general form and `canvas` the
    // one-screen shorthand for it; each entry is a terminal of the same kernel,
    // numbered by its position, with a shell and a console of its own
    // (Concept.md §3.5). See web/dual.html.
    //
    // `shell: false` on an entry leaves that terminal bare — a console but no
    // /bin/sh — for a screen a program opens rather than one anybody types at.
    const specs = options.screens && options.screens.length
        ? options.screens
        : [{ canvas: options.canvas, keys: options.keys }];

    for (const spec of specs) {
        if (!spec || !spec.canvas)
            throw new Error("braam: mount needs a canvas");
        if (!spec.canvas.transferControlToOffscreen)
            throw new Error("braam: this browser has no OffscreenCanvas");
    }

    const onLog = options.onLog || ((text) => console.log(text));
    const onError = options.onError || ((text) => console.error(text));

    const worker = new Worker(new URL(options.workerUrl || "./worker.js", import.meta.url),
                              { type: "module" });

    // Until the worker names a step of its own.
    let stage = "loading the worker";
    let spoke = false;
    const stall = setTimeout(() => {
        onError(`braam: boot is stuck ${stage} (${BOOT_STALL_MS / 1000}s).`
                + " A blocking extension or a proxy can hold a fetch open for ever;"
                + " try a private window or a profile with extensions off.");
    }, BOOT_STALL_MS);

    // First of all: the worker fetches nothing until it knows what to fetch.
    worker.postMessage({
        kind: "options",
        options: {
            wasmUrl: options.wasmUrl,
            rootfsUrl: options.rootfsUrl,
            procWorkerUrl: options.procWorkerUrl,
            palette: options.palette,
            fontFamily: options.fontFamily,
            fontSize: options.fontSize,
        },
    });

    // navigator.storage.persist() exists only on the main thread (Concept.md
    // §A.2), and asking for it is what keeps files from being evicted without
    // warning. Boot waits for it, so it is answered twice when it is slow: once
    // provisionally, once for real.
    let answered = false;
    const grace = setTimeout(() => {
        if (!answered)
            worker.postMessage({ kind: "persisted", value: false });
    }, PERSIST_GRACE_MS);

    persistOnce(options.persist !== false).then((granted) => {
        answered = true;
        clearTimeout(grace);
        worker.postMessage({ kind: "persisted", value: granted });
    });

    // What the panes of this mount share. A mutable record rather than
    // closures, because the clipboard wait moves between panes.
    const session = {
        worker,
        options,
        onLog,
        onError,
        live: true,
        pasteWaiter: null,
    };

    const screens = specs.map((spec, term) => makePane(session, spec, term));

    // Reading the clipboard is only allowed from inside a user-gesture handler,
    // and a command reaches this page long after its keystroke returned — so
    // the async API is refused by design in some browsers. A paste is itself
    // the gesture, and needs no permission at all, so a refused read waits for
    // one — on whichever pane has the focus when it arrives.
    function awaitPaste() {
        return new Promise((resolve) => {
            if (session.pasteWaiter)
                session.pasteWaiter(""); // a superseded wait answers empty, not never
            session.pasteWaiter = resolve;
        });
    }

    // The three host services a worker cannot perform itself: navigator.
    // clipboard, <input type="file"> and a download all need the DOM
    // (Concept.md §5.4). The worker asks by id and the answer goes straight
    // back. One of each for the mount: whichever screen asked, the file
    // dialogue and the clipboard are the page's.
    let picker = options.picker || null;
    let ownPicker = false;

    function filePicker() {
        if (!picker) {
            picker = document.createElement("input");
            picker.type = "file";
            picker.multiple = true;
            picker.hidden = true;
            document.body.appendChild(picker);
            ownPicker = true;
        }
        return picker;
    }

    function pick() {
        const input = filePicker();
        return new Promise((resolve) => {
            const done = async () => {
                const files = [...input.files];
                input.value = "";
                resolve(await Promise.all(files.map(async (f) => ({
                    name: f.name,
                    bytes: new Uint8Array(await f.arrayBuffer()),
                }))));
            };
            input.addEventListener("change", done, { once: true });
            input.addEventListener("cancel", () => {
                input.removeEventListener("change", done);
                resolve([]);
            }, { once: true });
            // The keystroke that ran the command is still the current
            // activation, which is what lets this open without a click of its
            // own.
            input.click();
        });
    }

    function fexport(name, bytes) {
        const url = URL.createObjectURL(new Blob([bytes]));
        const a = document.createElement("a");
        a.href = url;
        a.download = name;
        a.click();
        setTimeout(() => URL.revokeObjectURL(url), 10000);
    }

    async function service(data) {
        switch (data.svc) {
        case "clipRead":
            return await navigator.clipboard.readText();
        case "clipWait":
            return await awaitPaste();
        case "clipWrite":
            await navigator.clipboard.writeText(data.text);
            return null;
        case "pick":
            return await pick();
        case "fexport":
            fexport(data.name, data.bytes);
            return null;
        }
        throw new Error(`unknown service ${data.svc}`);
    }

    worker.onmessage = ({ data }) => {
        if (data.kind === "stage") {
            stage = data.text;
            return;
        }
        // Anything else means boot got far enough to speak.
        if (!spoke) {
            spoke = true;
            clearTimeout(stall);
        }
        if (data.kind === "selection") {
            const pane = screens[data.term >>> 0];
            if (pane)
                pane.deliver(data.text);
            return;
        }
        if (data.kind === "svc") {
            service(data)
                .then((value) => worker.postMessage({ kind: "svc-reply", id: data.id, value }))
                .catch((e) => worker.postMessage({
                    kind: "svc-reply",
                    id: data.id,
                    error: (e && (e.name === "NotAllowedError" || e.name === "SecurityError"))
                        ? E.PERM : E.IO,
                }));
            return;
        }
        if (data.kind === "stats")
            return; // whoever asked is listening for itself
        if (data.kind === "error")
            onError(data.text);
        else
            onLog(data.text);
    };

    worker.onerror = (e) => {
        clearTimeout(stall);
        onError(`worker error: ${e.message}`);
    };

    return {
        // The first screen's, so a one-screen embedder reads what it always did.
        canvas: screens[0].canvas,
        worker,
        screens,

        focus(at = 0) {
            if (screens[at])
                screens[at].focus();
        },

        // What the mouse has marked, which is what the copy chord writes.
        selection(at = 0) {
            return screens[at] ? screens[at].selection() : "";
        },

        // Everything this page attached, in one place: a host that swaps views
        // must be able to let go of a terminal completely.
        dispose() {
            session.live = false;
            clearTimeout(stall);
            for (const pane of screens)
                pane.dispose();
            if (ownPicker && picker.parentNode)
                picker.parentNode.removeChild(picker);
            worker.onmessage = null;
            worker.onerror = null;

            // Asked first, told after. A process is a worker of the
            // kernel's worker, and terminating a parent is specified to take
            // its children with it — but a leaked one is a core spinning for
            // the life of the page, which is too much to leave to a spec
            // nobody here can check. The timeout is the backstop for a kernel
            // worker too wedged to read the message.
            worker.postMessage({ kind: "dispose" });
            setTimeout(() => worker.terminate(), 0);
        },
    };
}
