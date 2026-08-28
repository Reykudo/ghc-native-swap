# Roadmap

The initial milestone intentionally excludes features that would enlarge the
loader before its safety properties are established.

## Next

- Choose project licensing, maintainer, and source-repository metadata before a
  Hackage upload; the initializer deliberately does not invent legal metadata.
- Signed manifests and artifact digest verification.
- Durable slot manifests and garbage collection of unreferenced artifacts.
- Prometheus metrics for compile latency, active generations, retained native
  mappings, swap latency, and cleanup failures.
- A Linux integration image with fixed GHC/package databases and resource
  limits.
- Soak tests lasting hours with RSS and `/proc/<pid>/maps` assertions.
- Track retired `PROT_NONE` virtual address space separately from physical RSS.
- A compiler-side rejection pass for user-declared `foreign export` and
  reloadable CAFs, beyond the current trusted-plugin contract.
- Report and upstream a GHC fix that removes `DYNAMIC_OBJECT` `nc_ranges` from
  `CheckUnload` correctly; remove address tombstones only after the fixed RTS
  passes the full matrix.

## Later

- Explicit compatibility modules and CI matrices for newer GHC releases.
- An external-process/RPC execution mode for untrusted plugins.
- Package-environment manifests instead of a flat package allowlist.
- Migration/state handoff hooks with strict host-owned state.

## Not planned for the core

- General expression evaluation like the historical `plugins` package.
- Automatic dependency installation from Hackage.
- Hiding unsafe FFI or hostile native code behind a claim of memory safety.
- Loading code built by a different GHC or package ABI set.
