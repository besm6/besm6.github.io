// The JS half of the kernel ABI. Every import is non-blocking; results come
// back through wake() (Concept.md §2.2). host.now and host.random are
// sanctioned exceptions.

// memory.grow detaches the ArrayBuffer, which kills any cached view
// (Concept.md §8.4). All access goes through view(), which re-derives.
export class Memory {
    constructor() {
        this.memory = null;
        this._u8 = null;
        this._u32 = null;
    }

    bind(memory) {
        this.memory = memory;
        this._u8 = null;
        this._u32 = null;
    }

    view() {
        if (this._u8 === null || this._u8.byteLength === 0)
            this._u8 = new Uint8Array(this.memory.buffer);
        return this._u8;
    }

    // The cell grid and the screen descriptor are read as u32s.
    u32() {
        if (this._u32 === null || this._u32.byteLength === 0)
            this._u32 = new Uint32Array(this.memory.buffer);
        return this._u32;
    }

    str(ptr, len) {
        return new TextDecoder().decode(this.view().subarray(ptr, ptr + len));
    }

    // Pages committed. This is the one memory figure a browser will give up: it
    // is the instance's own, read off the object rather than measured, and it is
    // what /proc publishes as a process's usage (doc/Release_Notes.md).
    pages() {
        return this.memory ? this.memory.buffer.byteLength / 65536 : 0;
    }
}

// getRandomValues refuses more than 65536 bytes at a time, so a large read is
// filled in spans.
function fillRandom(bytes, ptr, len) {
    for (let at = 0; at < len; at += 65536) {
        const take = Math.min(65536, len - at);
        crypto.getRandomValues(bytes.subarray(ptr + at, ptr + at + take));
    }
}

// `present` draws and returns; it must never call back into the kernel, which
// would re-enter the scheduler in the middle of the tick that called it. The
// same is true of the storage imports, which is why every one of their replies
// goes out on a promise (see fs.js).
export function makeImports(mem, sink, present, fs, svc) {
    return {
        host: {
            log(ptr, len) {
                sink(mem.str(ptr, len));
            },
            now() {
                return performance.now();
            },
            random(ptr, len) {
                fillRandom(mem.view(), ptr, len);
            },
            present(term, x, y, w, h) {
                present(term, x, y, w, h);
            },
            fs: fs.fs,
            fs_sync: fs.fs_sync,
            svc,
        },
    };
}
