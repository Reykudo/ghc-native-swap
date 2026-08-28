# Decisions

## Native shared objects

The first backend uses position-independent `.so` artifacts rather than
relocatable `.o` files. This avoids the old x86-64 small-code-model and
low-address fragmentation constraints. GHC tracks native mapped ranges and
performs reachability-aware final unload.

Artifacts use GNU ld `-Bsymbolic`, type-specific unit IDs, and handle-scoped
symbol lookup so repeated Haskell names cannot be interposed between live
generations.

## Export-free Haskell ABI v4

Two earlier designs were rejected by subprocess stress tests:

- v1 returned `StablePtr (input -> IO output)` from a Haskell foreign export;
- v2 invoked a Haskell foreign export with per-call stable pointers.

Both eventually failed under concurrent swap/unload with RTS corruption such as
`PAP object entered`, `SIGABRT`, or `SIGSEGV`. The cause is structural: GHC
registers every foreign export as an object-owned stable root and frees those
roots inside `unloadNativeObj`, leaving no cleanup GC between root removal and
the unload request.

ABI v3 used a generated C resolver and passed the original gate. It was
superseded because Haskell-to-Haskell loading needs neither generated C nor a
per-call stable-pointer protocol.

ABI v4 generates one ordinary Haskell constructor:

```haskell
typedBinding = Export Plugin.invoke
```

ABI magic and the actual whole-value `TypeRep` fingerprint are encoded in both
the identifier and generated unit ID. The host resolves that exact closure and
creates the sole generation-owned `StablePtr` root. Retirement can free the
root, collect while the object remains a linker root, and only then request
unload.

## Typed probe, not client claims

The compiler builds and runs a probe importing the submitted `invoke` binding.
The descriptor comes from its actual type, not a JSON name or caller-supplied
fingerprint. The host independently computes the expected type and constructs
the only accepted symbol. A wrong type or ABI version is an absent symbol and
is rejected before interpreting the raw address.

This detects accidental type and package drift. It does not protect against a
malicious compiler service that deliberately forges the expected symbol.

## Two invocation modes

The strict mode remains the conservative default:

```haskell
invoke :: NFData output => HotSwap input output -> input -> IO output
```

It leases one generation for one call and never exposes a function value.

The separate managed mode addresses zero and arbitrary curried arity:

```haskell
snapshotFunction :: DynamicFunction function => Dynamic function -> IO function
```

`DynamicFunction` recursively wraps `argument -> rest` and terminates at
`NFData result => IO result`. It is still direct shared-heap Haskell invocation;
no argument or result is serialized and no stable pointer is allocated per
call.

## Finalizer-backed wrappers, not raw closures

Returning a raw plugin function was rejected after an immediate `SIGSEGV`: the
host can retain a plugin closure or lazy PAP after the runtime believes its
generation is inactive.

Returning `Managed token value` was also rejected after a double free. The
token and lazy value are independent fields; extraction and GHC liveness
analysis can finalize the token while a plugin thunk survives.

The accepted design returns only host-owned recursive wrappers. Every wrapper
needs the same `ForeignPtr` token for its eventual terminal `withForeignPtr`, so
partial applications retain their generation. The token finalizer merely
decrements an STM count. A retirement thread performs actual cleanup after the
count reaches zero. Terminal results are forced to normal form before that
protection ends.

## Retired address tombstones

Repeated finalizer-driven lifecycles exposed a stock GHC bug: native ranges are
inserted into `CheckUnload` from `nc_ranges` but removed through the empty
`sections` array. A later `.so` at the same address can therefore resolve to a
freed `ObjectCode`, producing `CheckUnload.c:489` under the debug RTS and a
double free under the normal RTS.

The runtime records each artifact's Linux mappings and, after GHC's own
reachability-aware unload, permanently reserves freed ranges as anonymous
`PROT_NONE` mappings with `MAP_FIXED_NOREPLACE`. File pages and RSS are released;
64-bit virtual address space is intentionally traded for safety. Occupied
ranges are retried before later loads.

This workaround stays until an upstream GHC fix passes the same debug,
concurrent, and sequential stress tests.

## Retain CAFs conservatively

The runtime chooses `RetainCAFs` because `DontRetainCAFs` has an open GHC crash
report for dynamic software updating. A plugin that evaluates a CAF may retain
its mapping; retaining memory is preferable to unmapping live code. Valid
plugins avoid reloadable CAFs, and mapping assertions catch retention in tests.

## Separate compiler service

The runtime never embeds a `Ghc` session. Compilation and the executable ABI
probe run in a separate web-service process. Compiler crashes and leaks do not
pollute the long-lived runtime, and deployers can isolate arbitrary compile-time
code with OS controls.

## Small core

WAI, Warp, Aeson, HTTP client, and process dependencies live in the compiler
sublibrary. The runtime library owns only loading, shared-heap calls, lifetime
tracking, retirement, Linux address guards, and a transport-independent poller.
