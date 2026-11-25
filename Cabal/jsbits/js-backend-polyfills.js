// Polyfills for missing JavaScript RTS primitives in GHC 9.12 bindists.
//
// The `directory` boot library shipped with GHC 9.12.2 calls `fchmodat` (via
// `System.Directory.Internal.Posix.setModeAt`, used for example by
// `removePathForcibly` and `copyPermissions`), but the JavaScript runtime in
// the 9.12.2 bindists does not define the `h$fchmodat` primitive that the
// generated FFI stub expects, so such calls fail at runtime with:
//
//   ReferenceError: h$fchmodat is not defined
//
// Likewise, the `clock` package's `System.Clock` FFI stubs reference
// `h$CLOCK_*` constants that the runtime does not define. The generated code
// calls them as zero-argument functions, and the runtime's `h$clock_gettime`
// implementation ignores the clock id, so the numeric values below, which
// mirror Linux, are only used for completeness.
//
// These definitions follow the same conventions as the runtime's `h$openat`
// (in particular `h$calculate_at` for dirfd-relative paths). The `flags`
// argument of `fchmodat` (e.g. `AT_SYMLINK_NOFOLLOW`) is ignored, as node's
// `chmodSync` has no symlink-safe equivalent on Linux.
//
// This file can be removed once the GHC JavaScript bindist in use ships a
// runtime with a native `h$fchmodat` and the `h$CLOCK_*` constants.

function h$fchmodat(dirfd, path, path_off, mode, flags) {
  if (h$isNode()) {
    try {
      var p = h$calculate_at(dirfd, path, path_off);
      h$fs.chmodSync(p, mode);
      return 0;
    } catch (err) {
      h$setErrno(err);
      return -1;
    }
  } else {
    return h$unsupported(-1);
  }
}

// Finally, `GHC.Conc.getNumProcessors` FFI stub references
// `h$getNumberOfProcessors`, which the runtime does not define either.

function h$getNumberOfProcessors() {
  if (h$isNode()) {
    return h$os.cpus().length;
  }
  return 1;
}

// The `cryptohash-sha256` package implements the hashing via C FFI routines
// (`hs_cryptohash_sha256_*`). Those are compiled with Emscripten, but the
// FFI stubs expect `h$`-prefixed JavaScript functions, so they do not
// resolve. Implement them using node's crypto module instead. The context
// pointer is used as an opaque key to a hash object.

var h$sha256contexts = new Map();
var h$crypto = require("crypto");

function h$hs_cryptohash_sha256_init(ctx, ctx_off) {
  h$sha256contexts.set(ctx, h$crypto.createHash("sha256"));
  return 0;
}

function h$hs_cryptohash_sha256_update(ctx, ctx_off, data, data_off, len) {
  var hash = h$sha256contexts.get(ctx);
  hash.update(data.u8.subarray(data_off, data_off + len));
  return 0;
}

function h$hs_cryptohash_sha256_finalize(ctx, ctx_off, out, out_off) {
  var hash = h$sha256contexts.get(ctx);
  h$sha256contexts.delete(ctx);
  var digest = hash.digest();
  for (var i = 0; i < digest.length; i++) {
    out.u8[out_off + i] = digest[i];
  }
  return 0;
}

function h$CLOCK_REALTIME() {
  return 0;
}
function h$CLOCK_MONOTONIC() {
  return 1;
}
function h$CLOCK_PROCESS_CPUTIME_ID() {
  return 2;
}
function h$CLOCK_THREAD_CPUTIME_ID() {
  return 3;
}
function h$CLOCK_MONOTONIC_RAW() {
  return 4;
}
function h$CLOCK_REALTIME_COARSE() {
  return 5;
}
function h$CLOCK_MONOTONIC_COARSE() {
  return 6;
}
function h$CLOCK_BOOTTIME() {
  return 7;
}
