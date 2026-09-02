// The JS half of the host-services ABI (Concept.md §3.7). One import, `svc`,
// multiplexed by operation exactly as storage is — and one export in the other
// direction, `ref`, which is how a JS object reaches the kernel: the kernel
// hands out a table slot, the host deposits into it, and every later operation
// on that object arrives with the object itself as the fourth argument.
//
// fetch and WebSocket exist in a worker, so they happen here. The clipboard,
// the file picker and the download need the DOM, so those are relayed to the
// page and answered from there.

import { E, Request, statusOf, u32le } from "./abi.js";

export const OP = {
    CLOCK: 1,
    DROP: 2,
    FETCH: 3,
    READ: 4,
    WS_OPEN: 5,
    WS_SEND: 6,
    WS_RECV: 7,
    CLIP_READ: 8,
    CLIP_WRITE: 9,
    PICK: 10,
    PICK_NAME: 11,
    PICK_READ: 12,
    FEXPORT: 13,
    CLIP_WAIT: 14,
    HOST_INFO: 15,
    PROC_SPAWN: 16,
    PROC_STEP: 17,
    PROC_KILL: 18,
    VERIFY: 19,
    INFLATE: 20,
    PROC_SIGNAL: 21,
};

const utf8 = new TextEncoder();

// The one signature algorithm there is (Package_Management.md §8).
const ED25519 = { name: "Ed25519" };

// A request body and a clipboard string arrive as text; a response body and a
// picked file are bytes. Both ends of the wire are byte counts.
function bytesOf(v) {
    if (typeof v === "string")
        return utf8.encode(v);
    if (v instanceof Uint8Array)
        return v;
    return new Uint8Array(v);
}

// "arm" and "64" join into arm64, but "x86" and "64" are x86-64 and not x8664.
function architecture(hi) {
    if (!hi.architecture)
        return "";
    if (!hi.bitness)
        return hi.architecture;
    if (hi.architecture === "arm" && hi.bitness === "64")
        return "arm64";
    return `${hi.architecture}-${hi.bitness}`;
}

// Windows reports a platform version that is not a Windows version: 11 says
// 15.0.0. The mapping is published rather than guessed at — UA-CH gives 0 for
// 7/8/8.1, which it cannot tell apart, so those get no number at all.
function platformVersion(platform, version) {
    if (!version)
        return "";
    if (platform !== "Windows")
        return version;
    const major = Number.parseInt(version, 10);
    if (major >= 13)
        return "11";
    return major > 0 ? "10" : "";
}

// What the browser will state about itself, as `name value` lines. A blank line
// splits the two halves: above it is what the boot banner shows, below it is
// for /proc/host alone — the agent string wraps a row on its own and only says
// anything when the interpreted fields above are missing.
//
// No user-agent parsing. The precise fields belong to userAgentData, which is
// Chromium's alone; a UA string is a compatibility fiction, and Safari still
// claims "Intel Mac OS X 10_15_7" on Apple Silicon, so a parser would report a
// confident lie. A field nothing can answer is left out rather than filled in
// with "unknown". No CPU model and no clock rate appear here because no browser
// API discloses either.
async function describeHost() {
    const n = navigator;
    const top = [];
    const rest = [];
    // A table: `name value`, the value at column 9. /proc/host serves these as
    // they are and `uname` reads them back; the colon the boot banner shows is
    // added there, being presentation rather than part of the field.
    const say = (into, key, value) => {
        if (value)
            into.push(key.padEnd(9) + value);
    };

    const uad = n.userAgentData; // absent outside Chromium and off a secure origin
    if (uad) {
        let hi = {};
        try {
            hi = await uad.getHighEntropyValues(
                ["architecture", "bitness", "platformVersion", "uaFullVersion"]);
        } catch {
            // Refused, which the low-entropy brands below survive.
        }

        // One of these is deliberate nonsense, and the order of the rest is
        // deliberately varied. Chrome lists both "Chromium" and "Google Chrome",
        // so taking the first real one would name a different browser on
        // different loads: the specific brand is the informative one.
        const brands = (uad.brands || []).filter((b) => !/not.a.brand/i.test(b.brand));
        const brand = brands.find((b) => b.brand !== "Chromium") || brands[0];
        if (brand)
            say(top, "browser", `${brand.brand} ${hi.uaFullVersion || brand.version}`);
        if (uad.platform) {
            const version = platformVersion(uad.platform, hi.platformVersion);
            say(top, "os", version ? `${uad.platform} ${version}` : uad.platform);
        }
        say(top, "arch", architecture(hi));
    }

    if (n.hardwareConcurrency)
        say(top, "cpu", `${n.hardwareConcurrency} cores`);
    if (n.deviceMemory) // Chromium only, and rounded to a power of two
        say(top, "memory", `${n.deviceMemory} GB`);

    say(rest, "locale", n.language);
    try {
        say(rest, "timezone", Intl.DateTimeFormat().resolvedOptions().timeZone);
    } catch {
        // No Intl in this build; the line simply does not appear.
    }
    say(rest, "agent", n.userAgent);

    return `${top.join("\n")}\n\n${rest.join("\n")}\n`;
}

// The kernel's request spec for a fetch: a method line, header lines, a blank
// line, then the body.
function parseSpec(spec) {
    const nl = spec.indexOf("\n");
    const method = nl < 0 ? "GET" : spec.slice(0, nl);
    const rest = nl < 0 ? "" : spec.slice(nl + 1);
    const sep = rest.indexOf("\n\n");
    const head = sep < 0 ? rest : rest.slice(0, sep);
    const body = sep < 0 ? "" : rest.slice(sep + 2);

    const headers = {};
    for (const line of head.split("\n")) {
        const at = line.indexOf(":");
        if (at > 0)
            headers[line.slice(0, at).trim()] = line.slice(at + 1).trim();
    }
    return { method: method || "GET", headers, body };
}

// fetch rejects with a bare TypeError whether the server refused the origin or
// nothing answered at all. A no-cors retry tells the two apart: the request
// leaves the browser either way, so only a reachable server resolves it. The
// reply is opaque and useless to read, which is why this runs on the failure
// path alone, and the abort drops the body once the headers have settled it.
// The probe is a bare GET whatever the request was, since it asks about the
// server rather than about the method, and GET is the one that is safe to make
// twice.
async function answered(url) {
    const stop = new AbortController();
    try {
        await fetch(url, { mode: "no-cors", signal: stop.signal });
        return true;
    } catch {
        return false;
    } finally {
        stop.abort();
    }
}

function packHeaders(res) {
    let out = "";
    res.headers.forEach((value, name) => {
        out += `${name}: ${value}\n`;
    });
    return utf8.encode(out);
}

// A response whose headers have landed but whose body has not. `left` is the
// tail of the last chunk the kernel had no room for.
function makeBody(res) {
    return { reader: res.body ? res.body.getReader() : null, left: null, done: !res.body };
}

async function readBody(s, r) {
    while ((!s.left || s.left.length === 0) && !s.done) {
        const { value, done } = await s.reader.read();
        if (done)
            s.done = true;
        else
            s.left = value;
    }
    if (!s.left || s.left.length === 0) {
        r.set("bufLen", 0);
        r.set("status", 0);
        return;
    }
    s.left = s.left.subarray(r.writeSome(s.left));
}

// A socket plus the messages that arrived while nothing was reading. Every
// event resolves whoever is parked, so a receive never polls.
function makeSocket(url) {
    const s = { ws: new WebSocket(url), queue: [], closed: false, waiters: [] };
    s.ws.binaryType = "arraybuffer";

    const flush = () => {
        const w = s.waiters;
        s.waiters = [];
        for (const resolve of w)
            resolve();
    };

    s.ws.onmessage = (e) => {
        s.queue.push(bytesOf(e.data));
        flush();
    };
    s.ws.onclose = () => {
        s.closed = true;
        flush();
    };
    s.ws.onerror = () => {
        s.closed = true;
        flush();
    };
    return s;
}

function opened(s) {
    return new Promise((resolve, reject) => {
        if (s.ws.readyState === WebSocket.OPEN) {
            resolve();
            return;
        }
        s.ws.addEventListener("open", () => resolve(), { once: true });
        s.ws.addEventListener("error", () => reject({ braam: E.IO }), { once: true });
        s.ws.addEventListener("close", () => reject({ braam: E.IO }), { once: true });
    });
}

// The message at the head of the queue, left there: it is only consumed once
// the kernel has had room for it (abi.js's two-phase reply).
async function peek(s) {
    while (!s.queue.length && !s.closed)
        await new Promise((resolve) => s.waiters.push(resolve));
    return s.queue.length ? s.queue[0] : null;
}

// A clipboard reply is sized twice like every other variable-length one, and
// neither the async API nor a paste can be asked a second time — so the text is
// held until the kernel has room for it.
async function clipboard(r, held, key, get) {
    if (held[key] === null)
        held[key] = await get();
    const bytes = utf8.encode(held[key]);
    r.write(bytes);
    if (r.get("bufLen") !== 0 || bytes.length === 0)
        held[key] = null;
}

// Builds the import. `deposit` hands an object to the kernel's externref table;
// `relay` asks the page for something only the DOM can do; `reply` delivers a
// finished request. As in fs.js, `reply` must never run inside the import call
// itself — every path below goes through a promise, so it cannot.
export function makeSvcImport(mem, deposit, relay, reply, proc) {
    const held = { read: null, wait: null };

    async function perform(r, op, ref) {
        switch (op) {
        case OP.CLOCK: {
            const now = Date.now();
            r.set("flags", 1440 - new Date().getTimezoneOffset());
            r.ok(now >>> 0, Math.floor(now / 4294967296) >>> 0);
            return;
        }

        case OP.FETCH: {
            // A retry only wants more room for the headers; the object is
            // already in the slot, so the request is not made twice.
            let body = ref;
            if (!body) {
                const spec = parseSpec(r.text());
                const init = { method: spec.method, headers: spec.headers };
                if (spec.body.length && spec.method !== "GET" && spec.method !== "HEAD")
                    init.body = spec.body;
                const res = await fetch(r.arg(), init).catch(async () => {
                    throw { braam: (await answered(r.arg())) ? E.PERM : E.IO };
                });
                body = makeBody(res);
                body.status = res.status;
                body.headers = packHeaders(res);
                deposit(r.get("ref"), body);
            }
            r.set("resultLo", body.status);
            const cap = r.get("bufCap");
            if (body.headers.length > cap) {
                r.set("bufLen", 0);
                r.set("resultHi", body.headers.length);
                r.set("status", 0);
                return;
            }
            mem.view().set(body.headers, r.get("bufPtr"));
            r.set("bufLen", body.headers.length);
            r.set("resultHi", 0);
            r.set("status", 0);
            return;
        }

        case OP.READ:
            if (!ref) {
                r.fail(E.INVALID);
                return;
            }
            await readBody(ref, r);
            return;

        case OP.WS_OPEN: {
            const s = makeSocket(r.arg());
            await opened(s);
            deposit(r.get("ref"), s);
            r.ok();
            return;
        }

        case OP.WS_SEND:
            if (!ref || ref.closed) {
                r.fail(E.CLOSED);
                return;
            }
            ref.ws.send(r.text());
            r.ok();
            return;

        case OP.WS_RECV: {
            if (!ref) {
                r.fail(E.INVALID);
                return;
            }
            const msg = await peek(ref);
            if (msg === null) {
                r.set("flags", 1);
                r.set("bufLen", 0);
                r.set("status", 0);
                return;
            }
            r.write(msg);
            if (r.get("bufLen") === 0 && msg.length > 0)
                return; // no room; it stays queued for the retry
            ref.queue.shift();
            return;
        }

        case OP.CLIP_READ:
            await clipboard(r, held, "read", () => relay({ svc: "clipRead" }));
            return;

        case OP.CLIP_WAIT:
            await clipboard(r, held, "wait", () => relay({ svc: "clipWait" }));
            return;

        // Re-readable, unlike the clipboard, so there is nothing to hold across
        // the sized-twice retry: it is simply built again.
        case OP.HOST_INFO:
            r.write(utf8.encode(await describeHost()));
            return;

        case OP.CLIP_WRITE:
            await relay({ svc: "clipWrite", text: r.text() });
            r.ok();
            return;

        // No relay: crypto.subtle is in the worker.
        case OP.VERIFY: {
            const buf = r.bytes();
            const keyLen = u32le(buf, 0);
            const sigLen = u32le(buf, 4);
            const key = buf.subarray(8, 8 + keyLen);
            const sig = buf.subarray(8 + keyLen, 8 + keyLen + sigLen);
            const msg = buf.subarray(8 + keyLen + sigLen);
            let good;
            try {
                const k = await crypto.subtle.importKey("raw", key, ED25519, false, ["verify"]);
                good = await crypto.subtle.verify(ED25519, k, sig, msg);
            } catch (e) {
                // statusOf would otherwise call this IO.
                if (e && e.name === "NotSupportedError")
                    throw { braam: E.UNSUPPORTED };
                throw e;
            }
            if (good)
                r.ok();
            else
                r.fail(E.PERM);
            return;
        }

        // A reader like a fetch body's, so OP.READ and OP.DROP serve it
        // unchanged. A corrupt stream errors on a later read, not here.
        case OP.INFLATE: {
            const s = new Blob([r.bytes()]).stream()
                .pipeThrough(new DecompressionStream("deflate-raw"));
            deposit(r.get("ref"), { reader: s.getReader(), left: null, done: false });
            r.ok();
            return;
        }

        case OP.PICK: {
            const files = await relay({ svc: "pick" });
            deposit(r.get("ref"), files);
            r.ok(files.length);
            return;
        }

        case OP.PICK_NAME: {
            const f = ref && ref[r.get("flags")];
            if (!f) {
                r.fail(E.NOTFOUND);
                return;
            }
            r.write(utf8.encode(f.name));
            return;
        }

        case OP.PICK_READ: {
            const f = ref && ref[r.get("flags")];
            if (!f) {
                r.fail(E.NOTFOUND);
                return;
            }
            const off = r.get("resultLo") + r.get("resultHi") * 4294967296;
            r.writeSome(f.bytes.subarray(Math.min(off, f.bytes.length)));
            return;
        }

        case OP.FEXPORT:
            await relay({ svc: "fexport", name: r.arg(), bytes: r.bytes() });
            r.ok();
            return;

        // Isolated processes (proc.js). They are ops of this import rather
        // than an import of their own for the reason §2.2 gives: one import
        // per calling convention, and this is that convention.
        case OP.PROC_SPAWN:
            proc.spawn(r);
            return;

        // A step is answered when the process gets there, which is a message
        // away and may be never — a program in a loop between syscalls is killed
        // rather than waited for.
        case OP.PROC_STEP:
            await new Promise((done) => proc.step(r, done));
            return;

        default:
            r.fail(E.UNSUPPORTED);
        }
    }

    return function svc(op, token, req, ref) {
        // Killing a process is told, not asked, and `req` is the pid rather
        // than a record: there is nothing to reply to and nothing to free.
        if (op === OP.PROC_KILL) {
            proc.kill(req >>> 0);
            return;
        }

        // Told too, and for the same reason. The pid is `req` and the signal
        // rides in `token`, which no record-less op uses.
        if (op === OP.PROC_SIGNAL) {
            proc.signal(req >>> 0, token >>> 0);
            return;
        }

        // Letting go is told, not asked: there is no reply and no record.
        if (op === OP.DROP) {
            if (ref && ref.ws)
                ref.ws.close();
            else if (ref && ref.reader && !ref.done)
                ref.reader.cancel().catch(() => {});
            return;
        }

        const r = new Request(mem, req);
        perform(r, op, ref)
            .catch((e) => r.fail(statusOf(e)))
            .then(() => reply(token));
    };
}
