# Safety model

## Loader invariants

All of these invariants are required:

1. The host uses the threaded dynamic RTS; artifacts use `-dynamic`, `-fPIC`,
   `-fno-full-laziness`, Clang's integrated assembler, `-shared`,
   `-fno-link-rts`, and `-optl-Wl,-Bsymbolic`.
2. Host and artifact use the same full GHC patch version, target, RTS flavour,
   package databases, and shared package ABI set.
3. Every artifact path is absolute, unique, immutable, and never reused in one
   process. The system loader caches handles by pathname.
4. Native lookup is scoped to the returned handle, never the global namespace.
5. ABI magic and the actual probed `TypeRep` fingerprint are encoded in the
   Haskell closure symbol; lookup succeeds before the raw pointer is read.
6. A valid artifact has no Haskell `foreign export` and no generated C shim.
   Its ABI root is an ordinary `Export function` Haskell constructor.
7. A generation owns exactly one `StablePtr (Export function)` root. There is
   no per-call stable pointer.
8. Arguments and results cross as ordinary values in the shared Haskell heap;
   there is no runtime serialization.
9. Strict `invoke` calls hold an explicit generation lease. Managed snapshots
   hold a finalizer-backed token through every partial application.
10. Managed terminal actions use `withForeignPtr`; their finalizer cannot race
    a running call.
11. Plugin results reach normal form before either kind of call releases its
    protection.
12. Synchronous exceptions become a strict host-retained `String` while the
    generation is protected; asynchronous exceptions propagate.
13. Retirement waits for strict leases and managed tokens before freeing the
    generation root.
14. A major GC completes after root removal while the object is still a linker
    root. Only then does `unloadNativeObj` run, followed by another major GC.
15. On Linux, all retired file ranges are permanently reserved as anonymous
    `PROT_NONE` mappings before another artifact can load.

## Finalizers and laziness

The managed finalizer is deliberately small:

```text
ForeignPtr token becomes unreachable
        -> STM decrement of generationActive
        -> retirement worker eventually observes zero
```

It does not free the generation `StablePtr`, call the linker, collect the heap,
or unmap anything. The retirement worker still owns the generation and performs
those operations in order.

The lifetime token is safe only because callers never receive the raw plugin
value separately. Recursive host wrappers carry the token through
`argument -> ... -> IO result`, including every partial application. A sibling
structure such as `Managed token value` is unsafe: `value` may remain reachable
after `token` becomes dead, especially after demand/liveness optimisation.

`NFData.rnf` is required at the terminal result because WHNF is not enough. A
list, record, exception message, or other result can contain thunks whose entry
code, SRT, static data, or captured closures belong to the reloadable module.
Those thunks must be evaluated or fail while the generation is protected. An
incorrect or deliberately shallow `NFData` instance violates the contract.

Higher-order results are intentionally unsupported: standard function types do
not have `NFData`, and a plugin closure must not be smuggled through a custom
container or dishonest instance.

## GHC native unload defect

Stock GHC 9.10.3 has an unsafe repeated-unload path for `loadNativeObj` shared
objects:

- `insertOCSectionIndices` indexes a `DYNAMIC_OBJECT` through its `nc_ranges`;
- `removeOCSectionIndices` removes only `oc->sections` entries;
- dynamic objects have no entries in `oc->sections`, so their range index keeps
  a pointer to the freed `ObjectCode` after `dlclose`;
- if a later `.so` reuses the same virtual address, GC can resolve its static
  closure to the freed metadata.

The observed result under the debug RTS is
`ASSERTION FAILED: rts/CheckUnload.c, line 489`; the normal RTS reports a glibc
double free or may corrupt the linker. The same asymmetric removal code was
still present in GHC master, 9.12, and 9.14 when checked on 2026-08-24.

The runtime therefore records the artifact's `/proc/self/maps` ranges before
unload. After the reachability-aware post-unload GC, it reserves every freed
range using `mmap(PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS |
MAP_FIXED_NOREPLACE)`. A range that is still occupied remains in a retry queue;
the queue is retried under the linker lock before later loads. Stale GHC index
entries can no longer alias new code.

This workaround releases file-backed mappings and physical pages but consumes
retired virtual address space for the life of the process. It is suitable for
the supported 64-bit Linux target, not a portable fix for GHC. A future GHC
version must fix range removal and pass the full stress matrix before the
tombstones can be removed.

Relevant upstream source:

- [`rts/CheckUnload.c`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10.3-release/rts/CheckUnload.c)
- [`rts/linker/LoadNativeObjPosix.c`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10.3-release/rts/linker/LoadNativeObjPosix.c)

## CAF policy

The linker is initialized with `RetainCAFs`, not `DontRetainCAFs`. GHC issue
[#23182](https://gitlab.haskell.org/ghc/ghc/-/issues/23182) documents random
segfaults in dynamic-update systems using `DontRetainCAFs`; safe retention is
preferable to unmapping live code.

Reloadable modules must not evaluate or retain top-level CAFs. A retained CAF
may intentionally keep its `.so` mapped after retirement. The pending address
guard remains queued while that mapping exists. Prefer functions and strict
values; do not add `-fkeep-cafs`.

## Plugin author rules

- Define all boundary types in permanently loaded shared API packages.
- Define `invoke` as `Entry Request Response` or another curried type ending in
  `IO result`.
- Do not declare `foreign export` in a reloadable module.
- Give terminal results correct `NFData` instances; do not return functions,
  plugin-defined constructors, lazy streams, custom finalizers, or hidden
  plugin closures.
- Storing the host wrapper returned by `snapshotFunction` in an `IORef`, `MVar`,
  or `TVar` is supported. Storing a raw plugin closure is not.
- Do not leave unmanaged threads executing plugin code after a terminal action
  returns.
- Treat a blocked call or reachable partial application as retaining its
  generation.
- Treat `foreign import`, `unsafeCoerce`, raw addresses, and native libraries as
  trusted-code features capable of violating every invariant.

## Failure boundary

The host converts synchronous plugin exceptions to `PluginInvocationFailed`
while the generation is protected; no plugin exception value survives
retirement. This is strict exception rendering, not serialization.

The project prevents loader-induced use-after-unload only for well-typed,
cooperative plugins following this contract. It cannot make hostile FFI or
native code memory-safe. Untrusted code requires a separate process/container
or VM and an RPC runtime, not a shared in-process RTS.

## Host and platform

The host executable must include:

```cabal
ghc-options: -dynamic -threaded
```

The current unload backend requires 64-bit Linux, `/proc/self/maps`, glibc
`mmap`, and kernel support for `MAP_FIXED_NOREPLACE`. Plugin code may depend only
on shared packages in the configured package databases and allowlist. It must
not reference executable-only symbols such as `Main`.
