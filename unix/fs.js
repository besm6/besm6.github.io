// The JS half of the storage ABI (Concept.md §5.2). Two imports: `fs` starts an
// asynchronous operation and answers through wake(), and `fs_sync` is the
// sanctioned §2.2 exception — once a sync access handle exists, reading and
// writing a file really are synchronous.
//
// The request record itself is in abi.js, shared with host services.

import { E, Request, statusOf } from "./abi.js";

export { E, Request, statusOf };

export const OP = {
    INFO: 1,
    UNPACK: 2,
    OPEN: 3,
    STAT: 4,
    LIST: 5,
    MKDIR: 6,
    REMOVE: 7,
    TOUCH: 8,
    SYMLINK: 9,
    READLINK: 10,
    RENAME: 11,
};

// src/fs/fs.h's NodeKind, which a reply carries as a whole word.
export const KIND = { FILE: 0, DIR: 1, LINK: 2 };

// A symbolic link is a file whose whole contents are this magic and the target
// (Concept.md §5.2). Size rules one out before it is read.
export const LINK_MAGIC = "!<braamlink>";
export const LINK_TARGET_MAX = 1024;

// A whole file's link target, or null. The target must be the entire rest of
// the file, non-empty, within LINK_TARGET_MAX and free of NUL.
export function linkTarget(bytes) {
    if (bytes.length <= LINK_MAGIC.length ||
        bytes.length > LINK_MAGIC.length + LINK_TARGET_MAX)
        return null;
    for (let i = 0; i < LINK_MAGIC.length; i++)
        if (bytes[i] !== LINK_MAGIC.charCodeAt(i))
            return null;
    const rest = bytes.subarray(LINK_MAGIC.length);
    if (rest.includes(0))
        return null;
    return new TextDecoder().decode(rest);
}

export function linkBytes(target) {
    return new TextEncoder().encode(LINK_MAGIC + target);
}

export const SYNC = {
    READ: 1,
    WRITE: 2,
    SIZE: 3,
    TRUNCATE: 4,
    FLUSH: 5,
    CLOSE: 6,
};

// Open flags, from src/fs/fs.h. Only creation, truncation and exclusion reach
// OPFS: a sync access handle is read-write regardless of what the opener asked
// for.
export const O_CREATE = 4, O_TRUNC = 8, O_EXCL = 32;

// The reply the kernel decodes in fs_decode_entries: per entry, six u32s and
// then the name, padded up to the next word.
export function packEntries(entries) {
    let size = 0;
    const names = entries.map((e) => new TextEncoder().encode(e.name));
    for (const n of names)
        size += 24 + ((n.length + 3) & ~3);

    const out = new Uint8Array(size);
    const words = new Uint32Array(out.buffer);
    let at = 0;
    entries.forEach((e, i) => {
        const n = names[i];
        words[at >> 2] = e.link ? KIND.LINK : e.dir ? KIND.DIR : KIND.FILE;
        words[(at >> 2) + 1] = e.size >>> 0;
        words[(at >> 2) + 2] = Math.floor(e.size / 4294967296) >>> 0;
        words[(at >> 2) + 3] = (e.mtime || 0) >>> 0;
        words[(at >> 2) + 4] = Math.floor((e.mtime || 0) / 4294967296) >>> 0;
        words[(at >> 2) + 5] = n.length;
        out.set(n, at + 24);
        at += 24 + ((n.length + 3) & ~3);
    });
    return out;
}

// FsOp::Stat's reply, fixed width, decoded in OpfsFs::stat.
export function packStat(s) {
    const out = new Uint8Array(20);
    const words = new Uint32Array(out.buffer);
    words[0] = s.link ? KIND.LINK : s.dir ? KIND.DIR : KIND.FILE;
    words[1] = s.size >>> 0;
    words[2] = Math.floor(s.size / 4294967296) >>> 0;
    words[3] = (s.mtime || 0) >>> 0;
    words[4] = Math.floor((s.mtime || 0) / 4294967296) >>> 0;
    return out;
}

export function packInfo(info) {
    const out = new Uint8Array(32);
    const words = new Uint32Array(out.buffer);
    words[0] = info.quota >>> 0;
    words[1] = Math.floor(info.quota / 4294967296) >>> 0;
    words[2] = info.usage >>> 0;
    words[3] = Math.floor(info.usage / 4294967296) >>> 0;
    words[4] = info.opfs ? 1 : 0;
    words[5] = info.sync ? 1 : 0;
    words[6] = info.fsaccess ? 1 : 0;
    words[7] = info.persisted ? 1 : 0;
    return out;
}

// Splits "/a/b" into ["a", "b"]. Paths arrive already normalised.
function parts(path) {
    return path.split("/").filter((s) => s.length > 0);
}

// Where the unpacked archive's version is left, for boot to compare against
// the kernel's own (src/user/boot.cpp). The unpack replaces it with the rest
// of /etc.
export const VERSION_PATH = "/etc/version";

const u16 = (b, at) => b[at] | (b[at + 1] << 8);
const u32 = (b, at) => (u16(b, at) | (u16(b, at + 2) << 16)) >>> 0;

// A name out of an archive is not a path until it has been looked at: an
// absolute one, a drive letter or a `..` component would be written outside
// the root. Same-origin or not, this is the one zip bug worth checking by hand.
function safeName(name) {
    if (!name || name.startsWith("/") || name.includes("\\") || /^[A-Za-z]:/.test(name))
        return false;
    return !name.split("/").some((c) => c === "." || c === "..");
}

async function inflate(bytes) {
    const s = new Blob([bytes]).stream().pipeThrough(new DecompressionStream("deflate-raw"));
    return new Uint8Array(await new Response(s).arrayBuffer());
}

// rootfs.zip -> [{name, bytes}]. An ordinary deflated zip, read through the
// central directory: the end record points at it, and each of its entries
// points at a local header whose own name and extra lengths say where the data
// begins. Taking the central directory's offset for the data is the classic
// way to get this wrong.
//
// Async, because there is no synchronous inflate; installOps below is not, so
// a caller that cannot await inside a call can parse ahead of time.
export async function parseZip(bytes) {
    const bad = () => { throw { braam: E.INVALID }; };

    let eocd = -1;
    for (let at = bytes.length - 22; at >= 0 && at >= bytes.length - 22 - 0xffff; at--) {
        if (u32(bytes, at) === 0x06054b50) {
            eocd = at;
            break;
        }
    }
    if (eocd < 0)
        bad();

    const count = u16(bytes, eocd + 10);
    let at = u32(bytes, eocd + 16);
    // Zip64 announces itself by saturating these; nothing we pack comes near
    // either limit, so refusing is better than reading half an archive.
    if (count === 0xffff || at === 0xffffffff)
        throw { braam: E.UNSUPPORTED };

    const out = [];
    for (let i = 0; i < count; i++) {
        if (at + 46 > bytes.length || u32(bytes, at) !== 0x02014b50)
            bad();

        const flags = u16(bytes, at + 8);
        const method = u16(bytes, at + 10);
        const packed = u32(bytes, at + 20);
        const nameLen = u16(bytes, at + 28);
        const local = u32(bytes, at + 42);
        const name = new TextDecoder().decode(bytes.subarray(at + 46, at + 46 + nameLen));
        at += 46 + nameLen + u16(bytes, at + 30) + u16(bytes, at + 32);

        if (flags & 1)
            throw { braam: E.UNSUPPORTED }; // encrypted
        if (name.endsWith("/"))
            continue; // a directory entry; the paths below imply it anyway
        if (!safeName(name))
            bad();

        if (local + 30 > bytes.length || u32(bytes, local) !== 0x04034b50)
            bad();
        const from = local + 30 + u16(bytes, local + 26) + u16(bytes, local + 28);
        if (from + packed > bytes.length)
            bad();

        const data = bytes.subarray(from, from + packed);
        if (method === 0)
            out.push({ name, bytes: data.slice() });
        else if (method === 8)
            out.push({ name, bytes: await inflate(data) });
        else
            throw { braam: E.UNSUPPORTED };
    }
    return out;
}

// What installing an archive *is*, as operations for the caller to perform:
// OPFS awaits each one and the test fake does them synchronously, and neither
// has its own idea of what the archive means.
//
// The top-level directories the archive carries are removed first, so a name
// that is gone from a new release is gone from the store. Whatever it does not
// carry — /home, /tmp, /import — is never named and so never touched.
export function *installOps(entries, version) {
    const tops = new Set(entries.map((e) => e.name.split("/")[0]));
    for (const top of [...tops].sort())
        yield { op: "remove", path: "/" + top };

    const made = new Set();
    for (const e of entries) {
        const dirs = e.name.split("/").slice(0, -1);
        for (let i = 1; i <= dirs.length; i++) {
            const path = "/" + dirs.slice(0, i).join("/");
            if (!made.has(path)) {
                made.add(path);
                yield { op: "mkdir", path };
            }
        }
        yield { op: "write", path: "/" + e.name, bytes: e.bytes };
    }

    // Last, so an interrupted unpack leaves no stamp and is done again rather
    // than believed. Its directory is named here rather than assumed.
    const stampDir = VERSION_PATH.slice(0, VERSION_PATH.lastIndexOf("/"));
    if (stampDir && !made.has(stampDir))
        yield { op: "mkdir", path: stampDir };
    yield { op: "write", path: VERSION_PATH, bytes: new TextEncoder().encode(version) };
}

// The OPFS backend. Every method may reject; the caller turns that into a
// status, so a browser quirk becomes an error value rather than a dead kernel.
export class OpfsStore {
    constructor(root, rootfsUrl, persisted) {
        this.root = root;               // FileSystemDirectoryHandle, or null
        this.rootfsUrl = rootfsUrl;     // fetched only to unpack
        this.persisted = !!persisted;
        this.handles = [];              // index -> FileSystemSyncAccessHandle
        this.sync = typeof FileSystemFileHandle !== "undefined" &&
            !!FileSystemFileHandle.prototype.createSyncAccessHandle;
    }

    // A plain array rather than the externref table of Concept.md §3.7: the
    // table arrives with M6, and a handle is already just an index here.
    slot(handle) {
        return this.handles[handle] || null;
    }

    async dir(path, create) {
        let at = this.root;
        for (const name of parts(path))
            at = await at.getDirectoryHandle(name, { create });
        return at;
    }

    async info() {
        const est = (navigator.storage && navigator.storage.estimate)
            ? await navigator.storage.estimate()
            : {};
        return {
            quota: est.quota || 0,
            usage: est.usage || 0,
            opfs: !!this.root,
            sync: !!this.root && this.sync,
            fsaccess: typeof self.showDirectoryPicker === "function",
            persisted: this.persisted,
        };
    }

    async open(path, flags) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const create = (flags & O_CREATE) !== 0;
        const name = path.slice(at + 1);
        // getFileHandle with create is happy to succeed twice, as mkdir's is,
        // and there is no exclusive mode to ask for instead (Concept.md §5.2).
        if (flags & O_EXCL) {
            try {
                await dir.getFileHandle(name);
                throw { braam: E.EXISTS };
            } catch (e) {
                if (e && e.braam)
                    throw e;
            }
        }
        const file = await dir.getFileHandle(name, { create });
        const handle = await file.createSyncAccessHandle();
        if (flags & O_TRUNC)
            handle.truncate(0);

        let slot = this.handles.indexOf(null);
        if (slot < 0)
            slot = this.handles.push(null) - 1;
        this.handles[slot] = handle;
        return slot;
    }

    // A file's link target, or null. Only one small enough to be a link is read.
    async linkOf(file) {
        if (file.size <= LINK_MAGIC.length ||
            file.size > LINK_MAGIC.length + LINK_TARGET_MAX)
            return null;
        return linkTarget(new Uint8Array(await file.arrayBuffer()));
    }

    // A directory has no timestamp in OPFS, so it reports mtime 0. A link
    // reports itself; resolving one is the VFS's job.
    async stat(path) {
        if (path === "/")
            return { dir: true, size: 0, mtime: 0 };
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const name = path.slice(at + 1);

        // getFileHandle throws TypeMismatchError on a directory, so the
        // fallback is unconditional. A link mid-path is this.dir()'s NOTDIR.
        let file = null;
        try {
            file = await (await dir.getFileHandle(name)).getFile();
        } catch {
            await dir.getDirectoryHandle(name);
            return { dir: true, size: 0, mtime: 0 };
        }
        const target = await this.linkOf(file);
        return {
            dir: false,
            link: target !== null,
            size: target !== null ? target.length : file.size,
            mtime: file.lastModified,
        };
    }

    async list(path) {
        const dir = await this.dir(path, false);
        const out = [];
        for await (const [name, handle] of dir.entries()) {
            const isDir = handle.kind === "directory";
            const file = isDir ? null : await handle.getFile();
            const target = file ? await this.linkOf(file) : null;
            out.push({
                name,
                dir: isDir,
                link: target !== null,
                size: target !== null ? target.length : file ? file.size : 0,
                mtime: file ? file.lastModified : 0,
            });
        }
        return out;
    }

    async symlink(path, target) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const file = await dir.getFileHandle(path.slice(at + 1), { create: true });
        const handle = await file.createSyncAccessHandle();
        try {
            handle.truncate(0);
            handle.write(linkBytes(target), { at: 0 });
            handle.flush();
        } finally {
            handle.close();
        }
    }

    async readlink(path) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const file = await (await dir.getFileHandle(path.slice(at + 1))).getFile();
        const target = await this.linkOf(file);
        if (target === null)
            throw Object.assign(new Error("not a link"), { braam: E.INVALID });
        return target;
    }

    // OPFS cannot set a modification time, so the file is rewritten with its
    // own bytes and the browser restamps it. The stamp is read back: one that
    // did not move is reported rather than believed.
    async touch(path) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const name = path.slice(at + 1);
        const handle = await dir.getFileHandle(name);
        const before = (await handle.getFile()).lastModified;

        const h = await handle.createSyncAccessHandle();
        try {
            const n = h.getSize();
            const one = new Uint8Array(1);
            if (n > 0) {
                h.read(one, { at: 0 });
                h.write(one, { at: 0 });
            } else {
                h.write(one, { at: 0 });
                h.truncate(0);
            }
            h.flush();
        } finally {
            h.close();
        }

        if ((await handle.getFile()).lastModified === before)
            throw { braam: E.UNSUPPORTED };
    }

    async mkdir(path) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        const name = path.slice(at + 1);
        // getDirectoryHandle with create is happy to succeed twice, and mkdir
        // is not: an existing name has to be an error the shell can report.
        try {
            await dir.getDirectoryHandle(name);
            throw { braam: E.EXISTS };
        } catch (e) {
            if (e && e.braam)
                throw e;
        }
        await dir.getDirectoryHandle(name, { create: true });
    }

    // move() is implemented for files alone, and not in every engine, so a
    // directory and an engine without it are both UNSUPPORTED and the caller
    // copies. The destination goes first: a rename replaces, and engines differ
    // on whether move() will. A link is an ordinary file, so it moves as itself.
    async rename(from, to) {
        const fat = from.lastIndexOf("/");
        const fdir = await this.dir(from.slice(0, fat), false);

        let file;
        try {
            file = await fdir.getFileHandle(from.slice(fat + 1));
        } catch {
            throw { braam: E.UNSUPPORTED };
        }
        if (typeof file.move !== "function")
            throw { braam: E.UNSUPPORTED };

        const tat = to.lastIndexOf("/");
        const tdir = await this.dir(to.slice(0, tat), false);
        const name = to.slice(tat + 1);
        await tdir.removeEntry(name).catch(() => {});
        await file.move(tdir, name);
    }

    // A link is an ordinary file here, so removeEntry drops the link and not
    // its target, and a recursive remove cannot walk out through one.
    async remove(path, recursive) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), false);
        await dir.removeEntry(path.slice(at + 1), { recursive });
    }

    // A whole file at once, and a handle of its own: the unpack runs at boot
    // with nothing else open, so the exclusive lock is uncontended and the
    // handle never enters the table descriptors are drawn from.
    async put(path, bytes) {
        const at = path.lastIndexOf("/");
        const dir = await this.dir(path.slice(0, at), true);
        const file = await dir.getFileHandle(path.slice(at + 1), { create: true });
        const h = await file.createSyncAccessHandle();
        try {
            h.truncate(0);
            h.write(bytes, { at: 0 });
            h.flush();
        } finally {
            h.close();
        }
    }

    // Fetched here rather than at boot, so a system whose stored image is
    // current never asks for the archive at all.
    async unpack(version) {
        const res = await fetch(this.rootfsUrl);
        if (!res.ok)
            throw { braam: E.NOTFOUND };
        const entries = await parseZip(new Uint8Array(await res.arrayBuffer()));

        for (const step of installOps(entries, version)) {
            if (step.op === "remove")
                await this.remove(step.path, true).catch(() => {});
            else if (step.op === "mkdir")
                await this.dir(step.path, true);
            else
                await this.put(step.path, step.bytes);
        }
        return entries.length;
    }
}

// Builds the two imports. `reply` delivers a finished request to the kernel;
// it must never run inside the import call itself, or wake() would re-enter a
// tick that is still on the stack. Every path below goes through a promise,
// so it cannot.
export function makeFsImports(mem, store, reply) {
    async function perform(r, op) {
        switch (op) {
        case OP.INFO:
            r.write(packInfo(await store.info()));
            return;
        case OP.UNPACK:
            // The argument is the version to stamp, not a path.
            r.ok(await store.unpack(r.arg()));
            return;
        case OP.OPEN:
            r.ok(await store.open(r.arg(), r.get("flags")));
            return;
        case OP.STAT:
            r.set("status", 0);
            r.write(packStat(await store.stat(r.arg())));
            return;
        case OP.LIST:
            r.set("status", 0);
            r.write(packEntries(await store.list(r.arg())));
            return;
        case OP.MKDIR:
            await store.mkdir(r.arg());
            r.ok();
            return;
        case OP.REMOVE:
            await store.remove(r.arg(), (r.get("flags") & 1) !== 0);
            r.ok();
            return;
        case OP.TOUCH:
            await store.touch(r.arg());
            r.ok();
            return;
        case OP.SYMLINK:
            // The path is the argument, the target the buffer: two paths.
            await store.symlink(r.arg(), r.text());
            r.ok();
            return;
        case OP.READLINK:
            r.set("status", 0);
            r.write(new TextEncoder().encode(await store.readlink(r.arg())));
            return;
        case OP.RENAME:
            // The old path is the argument, the new one the buffer.
            await store.rename(r.arg(), r.text());
            r.ok();
            return;
        default:
            r.fail(E.UNSUPPORTED);
        }
    }

    return {
        fs(op, token, req) {
            const r = new Request(mem, req);
            if (!store.root && op !== OP.INFO) {
                Promise.resolve().then(() => {
                    r.fail(E.UNSUPPORTED);
                    reply(token);
                });
                return;
            }
            perform(r, op)
                .catch((e) => r.fail(statusOf(e)))
                .then(() => reply(token));
        },

        // Synchronous throughout, which is the point: a sync access handle
        // needs no promise, so no wake token is involved (Concept.md §5.2).
        fs_sync(op, handle, ptr, len, off) {
            const h = store.slot(handle);
            if (!h)
                return -E.INVALID;
            try {
                switch (op) {
                case SYNC.READ:
                    return h.read(mem.view().subarray(ptr, ptr + len), { at: off });
                case SYNC.WRITE:
                    return h.write(mem.view().subarray(ptr, ptr + len), { at: off });
                case SYNC.SIZE:
                    return h.getSize();
                case SYNC.TRUNCATE:
                    h.truncate(off);
                    return 0;
                case SYNC.FLUSH:
                    h.flush();
                    return 0;
                case SYNC.CLOSE:
                    // Flushing here rather than on every write: a sync handle
                    // buffers, and closing is the only point we know is safe.
                    try {
                        h.flush();
                    } catch { /* a read-only handle has nothing to flush */ }
                    h.close();
                    store.handles[handle] = null;
                    return 0;
                default:
                    return -E.UNSUPPORTED;
                }
            } catch (e) {
                return -statusOf(e);
            }
        },
    };
}

// Probes for OPFS. Returns a store either way: a root of null is what the
// kernel reads as "no OPFS", which it now reports as fatal (Concept.md §5.2).
// The archive is not fetched here — only an unpack needs it.
export async function openStore(rootfsUrl, persisted) {
    let root = null;
    try {
        if (navigator.storage && navigator.storage.getDirectory)
            root = await navigator.storage.getDirectory();
    } catch {
        root = null;
    }

    return new OpfsStore(root, rootfsUrl, persisted);
}
