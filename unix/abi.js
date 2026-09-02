// The shared half of the asynchronous host ABI: the request record, the error
// numbering, and the mapping from a browser rejection onto one of them. Storage
// (fs.js) and host services (svc.js) both speak it.
//
// The layout below mirrors struct HostRequest in src/kernel/hostcall.h field
// for field. Change one and change the other.

// src/kernel/result.h's Error, for the values these files report.
export const E = {
    INVALID: 1, NOMEMORY: 2, NOTFOUND: 3, EXISTS: 4, NOTDIR: 5, ISDIR: 6,
    PERM: 7, IO: 8, CANCELLED: 9, AGAIN: 10, UNSUPPORTED: 11, CLOSED: 12,
    NOTEMPTY: 13, LOOP: 14, INTR: 15,
};

// Field offsets in words, matching struct HostRequest.
const F = {
    op: 0, token: 1, argPtr: 2, argLen: 3, flags: 4,
    bufPtr: 5, bufCap: 6, status: 7, resultLo: 8, resultHi: 9, bufLen: 10,
    ref: 11, aux: 12,
};

export class Request {
    constructor(mem, addr) {
        this.mem = mem;
        this.at = addr >>> 2;
    }

    get(field) {
        return this.mem.u32()[this.at + F[field]];
    }

    set(field, value) {
        this.mem.u32()[this.at + F[field]] = value >>> 0;
    }

    // The inline string argument: a path, a URL, a file name.
    arg() {
        return this.mem.str(this.get("argPtr"), this.get("argLen"));
    }

    // The bytes the kernel put in the buffer for the host to read.
    bytes() {
        const ptr = this.get("bufPtr");
        return this.mem.view().slice(ptr, ptr + this.get("bufLen"));
    }

    text() {
        return this.mem.str(this.get("bufPtr"), this.get("bufLen"));
    }

    ok(lo = 0, hi = 0) {
        this.set("status", 0);
        this.set("resultLo", lo);
        this.set("resultHi", hi);
    }

    fail(code) {
        this.mem.u32()[this.at + F.status] = (-code) >>> 0;
    }

    // Copies a reply into the buffer the kernel supplied, or reports how much
    // room it would have needed (src/kernel/hostcall.h's two-phase reply).
    write(bytes) {
        const cap = this.get("bufCap");
        if (bytes.length > cap) {
            this.set("bufLen", 0);
            this.set("resultLo", bytes.length);
            this.set("status", 0);
            return;
        }
        this.mem.view().set(bytes, this.get("bufPtr"));
        this.set("bufLen", bytes.length);
        this.set("status", 0);
    }

    // The same, for a reply the host would rather not size twice: as much as
    // fits, with the remainder left for the next call.
    writeSome(bytes) {
        const cap = this.get("bufCap");
        const n = Math.min(bytes.length, cap);
        this.mem.view().set(bytes.subarray(0, n), this.get("bufPtr"));
        this.set("bufLen", n);
        this.set("status", 0);
        return n;
    }
}

// Maps a rejection onto one of src/kernel/result.h's Error values.
// A length word out of a staged payload, little-endian like every other field.
export function u32le(bytes, at) {
    return (bytes[at] | (bytes[at + 1] << 8) | (bytes[at + 2] << 16) | (bytes[at + 3] << 24)) >>> 0;
}

export function statusOf(e) {
    if (e && e.braam)
        return e.braam;
    const name = e && e.name;
    if (name === "NotFoundError")
        return E.NOTFOUND;
    if (name === "TypeMismatchError")
        return E.NOTDIR;
    if (name === "InvalidModificationError")
        return E.NOTEMPTY;
    if (name === "NoModificationAllowedError")
        return E.PERM;
    if (name === "NotAllowedError" || name === "SecurityError")
        return E.PERM;
    if (name === "QuotaExceededError")
        return E.IO;
    return E.IO;
}
