# Project documentation

The repository is a small modern replacement for the loading part of
[`plugins`](https://hackage.haskell.org/package/plugins) and the generation
management demonstrated by
[`ghc-hotswap`](https://github.com/fbsamples/ghc-hotswap).

Current scope:

- compile versioned Haskell modules in a separate HTTP service;
- load native position-independent `.so` artifacts into the application's RTS;
- atomically route new calls to a new generation;
- let calls already using the old generation finish;
- resolve a type-addressed ordinary Haskell closure with no generated C shim;
- keep the original strict one-call `invoke` API;
- expose a separate GC-managed snapshot API for zero or arbitrary curried
  arguments ending in `IO`;
- pass arguments and results directly through the shared Haskell heap, with no
  serialization or per-call stable pointers;
- use two-phase GC-aware native object retirement;
- reserve retired Linux virtual ranges to work around stale GHC native-object
  indices without retaining file pages;
- poll for a new artifact, normally once per minute;
- exercise loader failures and repeated swaps in subprocess stress tests so a
  segfault is reported as a test failure instead of killing the whole suite.

Documents:

- [`architecture.md`](architecture.md) — components, lifecycle, and API shape;
- [`safety.md`](safety.md) — invariants, limits, and deployment requirements;
- [`compiler-service.md`](compiler-service.md) — HTTP contract and compiler
  configuration;
- [`testing.md`](testing.md) — validation strategy and acceptance gates;
- [`decisions.md`](decisions.md) — decisions and rejected alternatives;
- [`roadmap.md`](roadmap.md) — intentionally deferred work.

The first supported platform is Linux x86-64 with GHC 9.10.3, Clang available
as `clang`, glibc, `/proc/self/maps`, and `MAP_FIXED_NOREPLACE`. GHC's linker
API is versioned with GHC, so another compiler release must be added and tested
explicitly rather than assumed.

The current ABI is Haskell-only v4. The compiler generates an ordinary
`Export Plugin.invoke` binding whose symbol and unit ID contain the probed
whole-type `TypeRep` fingerprint. The runtime resolves that exact symbol before
interpreting the closure address. Exact test evidence and commands are recorded
in [`testing.md`](testing.md).
