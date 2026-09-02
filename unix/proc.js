// The host's half of a process (Concept.md §4), and the process-side half of
// the protocol beside it, so the two cannot drift.
//
// An isolated process is a second WebAssembly.Instance with a memory the kernel
// sized, an import closure bound to its pid, and an externref table of its own
// that it gets for free by being a separate module. It lives in a worker of its
// own, where terminate() is a kill that does not need its cooperation. There is
// no second place to put one: a host that cannot make a worker is told so, and
// the kernel waits rather than running the process here (§4).
//
// The rule everything here follows: **a process never runs while the kernel is
// on the stack.** Its exports reach the kernel through `sys`, and re-entering
// the kernel in the middle of a tick would run it on a heap it is halfway
// through changing. A step is a message, which is deferred by nature.
// Compiling and instantiating execute no wasm, so they need no such care.

import { E } from "./abi.js";
import { Memory } from "./host.js";

// src/kernel/sysabi.h's ProcStep.
export const STEP = { EXITED: 0, SUSPENDED: 1, TRAPPED: 2 };

// src/kernel/sysabi.h's Sys. The synchronous half is answered in the process's
// own worker, because a worker boundary has no synchronous direction, and the
// asynchronous half rides back on the step's reply (Concept.md §4.3).
const SYS = { EXIT: 1, GETPID: 2, NOW: 3, STAGE: 4, RANDOM: 5 };

// src/kernel/sysabi.h's proc_pack: the two page counts in one word.
const initialOf = (flags) => flags & 0xffff;
const maxOf = (flags) => flags >>> 16;

// How many workers stay hired with no process in them: a four-stage pipeline's
// worth, so a pipeline neither hires nor terminates. The pool saves the cost of
// starting one, not memory: a process's pages go when its instance does, which
// is at the step that ended it and not at the next bind.
const MAX_IDLE = 4;

// Workers hired before anything needs one: the capability probe, and one for
// the shell, which takes one at boot and holds it for the session.
const PRE_HIRE = 2;

// ------------------------------------------------------- the process's half

// One instance, and the two calls it can make. `ops` answers what a worker can
// answer on its own and records the rest for the step's reply.
export function serveProc(ops) {
    const mem = new Memory();
    let instance = null;
    let started = false;

    // The instance and its memory go together, and nothing reads either once a
    // step has ended the process. A pooled worker keeps its `serveProc` until
    // it is hired again, so this is what stops it holding the last process's
    // pages until then.
    function release() {
        instance = null;
        mem.bind(null);
    }

    return {
        mem,

        // A worker out of the pool is already clean; this is the reset one gets
        // anyway, since a bind must not inherit an instance either way.
        bind(module, initial, maximum) {
            instance = null;
            started = false;
            const memory = new WebAssembly.Memory({ initial, maximum });
            mem.bind(memory);
            instance = new WebAssembly.Instance(module, {
                env: { memory },
                kernel: {
                    sys(op, a0, a1, a2) {
                        return ops.sys(op >>> 0, a0 >>> 0, a1 >>> 0, a2 >>> 0);
                    },

                    // The payload lives in the process's memory and the kernel
                    // cannot reach it, so the copy happens on the way out
                    // (Appendix B). The token goes with it: a process may have
                    // several calls outstanding, and the kernel says which one
                    // it is answering when it steps.
                    sys_async(op, tok, ptr, len) {
                        ops.sysAsync(op >>> 0, tok >>> 0, ptr >>> 0, len >>> 0, mem);
                    },
                },
            });
        },

        // One _start or _resume. `payload` is argv the first time and a syscall
        // reply afterwards, and `token` names the call it answers; it goes into
        // the process's own heap through its own allocator, and _resume frees
        // it.
        step(token, payload) {
            if (!instance)
                return { result: STEP.TRAPPED };

            let ptr = 0;
            if (payload.length) {
                ptr = instance.exports._alloc(payload.length) >>> 0;
                if (!ptr)
                    return { fail: E.NOMEMORY };
                mem.view().set(payload, ptr);
            }

            try {
                const out = started
                    ? instance.exports._resume(token >>> 0, ptr, payload.length)
                    : instance.exports._start(ptr, payload.length);
                started = true;

                // Read before any release below, and here rather than in the
                // kernel: only this side holds the instance. A process grows its
                // memory while it runs, so the step that ends is when the figure
                // has just changed.
                const pages = mem.pages();
                if (out === 0) {
                    // The process is over, so its memory goes now rather than
                    // at the next bind: a pooled worker holds its instance and
                    // would pin as much as PROC_MAX_PAGES until it is hired.
                    release();
                    return { result: STEP.EXITED, pages };
                }
                return { result: STEP.SUSPENDED, pages };
            } catch {
                // A trap is how a process reports a fatal error, and there is
                // nothing left to resume: the instance goes.
                const pages = mem.pages();
                release();
                return { result: STEP.TRAPPED, pages };
            }
        },

        // A signal. `_sig` is a leaf call — a word set, nothing to report — so
        // there is no reply. Before _start there is nobody to tell, and a trap
        // is left for the next step, which has somewhere to report it.
        signal(sig) {
            if (!instance || !started)
                return;
            try {
                instance.exports._sig(sig >>> 0);
            } catch {
                // Left for the next step, which has somewhere to report it.
            }
        },
    };
}

// The other half of `ops`, running in the process's own worker. Nothing here
// reaches the kernel: `getpid` is the pid the host bound in, `now` is a clock
// the step message carried, `random` is this worker's own CSPRNG, `exit` and the
// asynchronous call ride back on the step's reply, and `stage` is refused — it
// is the host's syscall rather than a program's, and a hostile binary may still
// call it.
//
// `clock` and `entropy` are what this cannot make for itself, and both are
// parameters for one reason: the tests hand it a frozen clock and a fixed
// stream.
export function workerOps(pid, clock, entropy) {
    let base = 0;
    let at = 0;
    let exit;
    let calls = [];

    return {
        sys(op, a0) {
            switch (op) {
            case SYS.EXIT:
                exit = a0 | 0;
                return 0;
            case SYS.GETPID:
                return pid;
            case SYS.NOW:
                return (base + (clock() - at)) >>> 0;
            case SYS.STAGE:
                return 0;
            // Every bit pattern is a draw, the top one included: nothing here
            // is a status, so a value arriving as -1 is a number.
            case SYS.RANDOM:
                return entropy() >>> 0;
            default:
                return -E.UNSUPPORTED;
            }
        },

        sysAsync(op, token, ptr, len, mem) {
            // slice, not subarray: the buffer is transferred, and a later
            // memory.grow would detach a view onto the instance's own memory
            // (Concept.md §8.4).
            //
            // A list, because one step can park more than one task: resuming
            // the root may start a second and leave both waiting.
            calls.push({ op, token, len, payload: mem.view().slice(ptr, ptr + len).buffer });
        },

        begin(now) {
            base = now;
            at = clock();
            exit = undefined;
            calls = [];
        },

        end() {
            return { exit, calls };
        },
    };
}

// ---------------------------------------------------------- the host's half

// `makeLink()` makes a worker for a process. Where it is absent or will not
// make one, a spawn is refused with AGAIN and the kernel backs off and asks
// again — there is nowhere else to put a process (Concept.md §4).
export function makeProc(mem, kernel, makeLink, clock = () => 0) {
    const modules = new Map(); // path -> Module; §4.4's compile cache
    const procs = new Map();   // pid -> the kernel's half of one process
    const idle = [];           // workers with no process in them

    // What a process costs, counted where it is spent. A round trip is one
    // asynchronous call and the step that answers it; the synchronous four are
    // answered in the worker and are not round trips.
    const stat = {
        calls: 0, steps: 0, signals: 0,
        spawned: 0, compiled: 0, hired: 0, reused: 0, terminated: 0, broke: 0,
    };

    // A link is one worker. It is hired without a process in it, which is also
    // the capability probe: where nested workers do not exist the constructor
    // throws here, at boot, rather than under the first `exec`.
    //
    // No latch behind it: the kernel paces the asking now (Concept.md §4), so a
    // host that could not make one a second ago is asked again rather than
    // written off for the life of the page.
    function hire() {
        if (!makeLink)
            return null;
        try {
            const link = makeLink();
            link.onmessage = ({ data }) => deliver(link, data);
            link.onerror = () => broke(link);
            link.pid = 0;
            stat.hired++;
            return link;
        } catch {
            return null;
        }
    }

    function take() {
        const link = idle.pop();
        if (link)
            stat.reused++;
        return link || hire();
    }

    function pool(link) {
        link.pid = 0;
        if (idle.length < MAX_IDLE) {
            idle.push(link);
        } else {
            stat.terminated++;
            link.terminate();
        }
    }

    // A worker that failed to load or threw where nothing could catch it. Its
    // process is a crashed one, and the link goes rather than being pooled —
    // hence the `p.link = null`, which is what stops kill() from handing a
    // dead worker back to the next process.
    function broke(link) {
        stat.broke++;
        const at = idle.indexOf(link);
        if (at >= 0)
            idle.splice(at, 1);
        const p = procs.get(link.pid);
        if (p && p.link === link) {
            p.link = null;
            finish(p, { result: STEP.TRAPPED });
        }
        link.pid = 0;
        stat.terminated++;
        link.terminate();
    }

    function spawn(r) {
        const pid = r.get("aux");
        const path = r.arg();
        const flags = r.get("flags");
        const initial = initialOf(flags);
        const maximum = maxOf(flags);

        // Compiled before a worker is asked for: the cache is the host's
        // (§4.4), a Module is structured-cloneable, and a malformed binary is
        // caught where `exec` can still say so rather than being retried.
        stat.spawned++;
        let module = modules.get(path);
        if (!module) {
            stat.compiled++;
            module = new WebAssembly.Module(r.bytes());
            modules.set(path, module);
        }

        // No worker, no process: there is nowhere else to put one. AGAIN rather
        // than a failure, because the kernel's answer is to wait and ask again
        // (Concept.md §4), and the image it sent is still on its side.
        const link = take();
        if (!link) {
            r.fail(E.AGAIN);
            return;
        }

        // The cap is the kernel's word and not the binary's: the module
        // declares no maximum of its own, so memory.grow stops at the number
        // that came over — which is an rlimit without cgroups (§4.1).
        link.pid = pid;
        link.postMessage({ k: "bind", pid, module, initial, maximum });
        procs.set(pid, { pid, link, pending: null, done: false });
        r.ok();
    }

    // `done()` answers the request. It must never reach wake() on this stack —
    // kill() is called from inside a host import — so both callers of makeProc
    // defer it.
    function step(r, done) {
        const pid = r.get("aux");
        const p = procs.get(pid);

        if (!p || !p.link || p.done) {
            r.fail(E.NOTFOUND); // killed while this step was in flight
            done();
            return;
        }

        stat.steps++;
        p.pending = { r, done };
        const payload = r.bytes();
        p.link.postMessage(
            { k: "step", now: clock(), token: r.get("flags"), payload: payload.buffer },
            [payload.buffer]);
    }

    // A signal, posted rather than called: only the process's worker reaches its
    // exports. Nothing is answered, so this leaves `pending` alone and may be
    // sent while a step is in flight — the worker is single-threaded, so it
    // lands between two of them.
    function signal(pid, sig) {
        const p = procs.get(pid);
        if (!p || !p.link || p.done)
            return; // killed, or already over: there is nobody to tell
        stat.signals++;
        p.link.postMessage({ k: "sig", sig });
    }

    // A step whose result was the process's last: the instance is gone, and the
    // worker that held it is clean enough to hire out again.
    function retire(p) {
        p.done = true;
    }

    // The reply, a message after the step: the exit status the process
    // reported, then the asynchronous call it parked on, then the answer to the
    // step itself.
    function finish(p, m) {
        const pending = p.pending;
        p.pending = null;

        if (m.fail !== undefined) {
            retire(p);
            if (pending) {
                pending.r.fail(m.fail);
                pending.done();
            }
            return;
        }

        if (m.exit !== undefined)
            kernel().sys(p.pid, SYS.EXIT, m.exit, 0, 0);

        // One step can park more than one task, so the calls come back as a
        // list. The token is the process's own and rides with each: the kernel
        // will name it again when it answers.
        for (const call of m.calls || []) {
            stat.calls++;
            const dst = kernel().sys(p.pid, SYS.STAGE, call.len, 0, 0) >>> 0;
            if (dst && call.len)
                mem.view().set(new Uint8Array(call.payload), dst);
            kernel().sys_async(p.pid, call.op, call.token, call.len);
        }

        if (m.result !== STEP.SUSPENDED)
            retire(p);
        if (pending) {
            // result_hi is the pages the instance has committed: the kernel has
            // no way to ask, and a step is already a message (Concept.md §4.3).
            pending.r.ok(m.result, m.pages || 0);
            pending.done();
        }
    }

    function deliver(link, m) {
        // A worker announcing that its script evaluated. Nothing acts on it now
        // that the pool has no latch to arm: a worker that will not load is a
        // process that died, and the kernel decides what to do about that.
        if (m.k === "ready")
            return;
        if (m.k !== "step")
            return;
        const p = procs.get(link.pid);
        if (p && p.link === link)
            finish(p, m);
    }

    // Terminating the worker a process is in, and answering for it. The reply
    // is not optional: an abandoned record is reaped by wake() on its token,
    // so a request nobody answers is leaked and its caller parked for ever.
    // Both ways a worker is taken away come through here.
    function terminate(p) {
        p.link.pid = 0;
        stat.terminated++;
        p.link.terminate();

        if (p.pending) {
            const { r, done } = p.pending;
            p.pending = null;
            r.fail(E.NOTFOUND);
            done();
        }
    }

    // Told, not asked, and immediate: the worker goes, which is the whole point
    // of it — a process in a loop between syscalls has no other way out. A
    // worker that had already finished its process is pooled instead, since
    // `exec` kills every process it spawned, including the ones that exited.
    function kill(pid) {
        const p = procs.get(pid);
        if (!p)
            return;
        procs.delete(pid);
        if (!p.link)
            return;

        if (p.done && !p.pending)
            pool(p.link);
        else
            terminate(p);
    }

    // Letting go of everything, for a host that is disposing of the kernel.
    function shutdown() {
        dropWorkers();
        procs.clear();
    }

    // Letting go of the workers alone: the pool, and every process one is
    // holding — which is all of them, the shell included, so init replaces it
    // and the replacement waits for a worker if there is none to be had
    // (Concept.md §4). A process losing its worker here is killed by it, step
    // and all. Not `shutdown`, because the kernel stays.
    function dropWorkers() {
        for (const link of idle) {
            stat.terminated++;
            link.terminate();
        }
        idle.length = 0;
        for (const [pid, p] of procs)
            if (p.link) {
                procs.delete(pid);
                terminate(p);
            }
    }

    function live() {
        return procs.size;
    }

    function pooled() {
        return idle.length;
    }

    function stats() {
        return { ...stat, pooled: idle.length, live: procs.size };
    }

    // Workers hired before anything needs one: the first `exec`s then cost an
    // instantiation rather than a worker start. The pool grows past this by
    // returning what it hired on demand, so PRE_HIRE is about the first command
    // and MAX_IDLE about the rest.
    for (let i = 0; i < PRE_HIRE; i++) {
        const link = hire();
        if (!link)
            break;
        idle.push(link);
    }

    return { spawn, step, signal, kill, shutdown, dropWorkers, live, pooled, stats };
}
