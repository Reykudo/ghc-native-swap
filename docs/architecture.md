# Architecture

## Components

The package has two libraries and one executable:

1. `ghc-native-swap` owns native generations, invocation leases, managed
   function snapshots, and retirement.
2. `ghc-native-swap:compiler` builds artifacts, implements the HTTP protocol,
   and downloads published revisions.
3. `ghc-native-swap-compiler` runs compilation as a separate web service.

Boundary values use `Control.DeepSeq.NFData`; no loader-specific value class or
support sublibrary is required. Compiler, JSON, HTTP, and process dependencies
stay in the compiler sublibrary.

## Plugin source contract

Source submitted to the compiler defines one binding named `invoke`. Its type
may be the strict convenience entry type:

```haskell
module Plugin where

import GHC.NativeSwap.Plugin (Entry)

invoke :: Entry Request Response
invoke request = ...
```

or any curried function shape ending in `IO`:

```haskell
invoke :: Int -> Text -> IO Result
invoke first second = ...

-- An effectful binding with zero ordinary arguments.
invoke :: IO Result
invoke = ...
```

`Entry input output` is only a synonym for `input -> IO output`. Arguments and
results remain in the same Haskell heap: there is no encoding, byte copy, RPC,
FFI call boundary, or per-invocation `StablePtr` protocol.

All boundary types must come from permanently loaded shared packages. The
reloadable module must not declare `foreign export`. The compiler-generated
artifact contains only normal Haskell bindings and deliberately has no
generated C source.

## Public runtime APIs

The original strict API remains the default:

```haskell
type HotSwap input output = Dynamic (input -> IO output)

invoke
  :: (NFData output, NonFunctionResult output)
  => HotSwap input output
  -> input
  -> IO output
```

`invoke` acquires and releases a generation lease for one call. No reloadable
function is returned to the host.

The managed API is separate and supports arbitrary arity:

```haskell
newDynamic
  :: Typeable function
  => FilePath
  -> IO (Dynamic function)

snapshotFunction
  :: DynamicFunction function
  => Dynamic function
  -> IO function

instance (NFData result, NonFunctionResult result) => DynamicFunction (IO result)
instance DynamicFunction rest => DynamicFunction (argument -> rest)
```

Examples:

```haskell
zero <- newDynamic artifact0 :: IO (Dynamic (IO Int))
action <- snapshotFunction zero
answer <- action

many <- newDynamic artifact2 :: IO (Dynamic (Int -> Text -> IO Result))
function <- snapshotFunction many
result <- function 10 "rules"
```

The recursive instances create host-owned wrappers for every partial
application. The same lifetime token is propagated through each wrapper. The
terminal `IO result` uses `withForeignPtr`, catches synchronous exceptions, and
runs `rnf` while the token is alive.

The boundary uses the result type's ordinary `NFData` instance. Its law is part
of the loader contract: after `rnf` returns, the value must retain no code, info
tables, static data, finalizers, or closures owned by the reloadable module.
Host API packages can represent an opaque value with a dedicated newtype and a
deliberately shallow `NFData` instance when permanent host ownership establishes
that invariant. The loader has no special instance for raw
`Data.Dynamic.Dynamic`. `NonFunctionResult` rejects direct function results even
though `deepseq` has a deprecated shallow `NFData (a -> b)` instance.

## Compiler pipeline

For every immutable generation the service:

1. writes the submitted module into an isolated staging directory;
2. builds and runs a small ABI probe importing the actual `invoke` binding;
3. obtains the GHC `TypeRep` fingerprint for its complete type;
4. generates `GHCNativeSwapGenerated` with a binding and unit ID containing ABI magic
   and both fingerprint words;
5. builds a `-dynamic -fPIC -fno-full-laziness -pgma clang -shared
   -fno-link-rts` artifact;
6. atomically renames and publishes the successful `.so`.

The generated Haskell-only ABI v4 binding is conceptually:

```haskell
module GHCNativeSwapGenerated (hotSwapInvokeA<magic>B<fp1>C<fp2>) where

import qualified Company.Rules.Plugin as Plugin
import GHC.NativeSwap.Plugin (Export (Export))

{-# NOINLINE hotSwapInvokeA<magic>B<fp1>C<fp2> #-}
hotSwapInvokeA<magic>B<fp1>C<fp2> = Export Plugin.invoke
```

Angle-bracket fields describe generated decimal identifier fragments, not
literal Haskell syntax. The host derives the exact closure symbol from its own
expected type. A missing or differently typed binding fails lookup before any
pointer cast.

There is no `foreign export` wrapper. GHC records every foreign export as an
object-owned stable root and frees those roots inside `unloadNativeObj`, too
late for the required cleanup GC.

The host and artifacts use one threaded dynamic RTS. Artifacts never link a
second RTS. Handle-scoped lookup, type-specific unit IDs, and `-Bsymbolic`
prevent interposition between simultaneously mapped generations.

## Runtime lifecycle

On load, the runtime resolves `Export function` and creates one generation-wide
`StablePtr` root. A managed snapshot increments the generation active count and
returns only host wrappers, never the raw plugin closure.

```text
compile g2 at a new immutable path
              |
              v
load g2, record mappings, resolve typed Export, root it once
              |
              v
atomically replace current g1 with g2
       |                         |
       |                         +--> new calls/snapshots use g2
       v
wait for strict calls and managed tokens on g1 to reach zero
              |
              v
block new native calls and drain committed terminal calls
              |
              v
free g1 StablePtr; major GC while g1 is still a linker root
              |
              v
unloadNativeObj(g1); major GC performs reachability-aware dlclose
              |
              v
reserve the retired virtual ranges as anonymous PROT_NONE tombstones
```

The first major GC removes transient plugin PAPs and thunks while the RTS still
treats the native object as an unconditional root. The following GC may unmap
it only after normal code-reachability marking.

The finalizer attached to a managed token performs no linker work. It only
decrements the generation active count. A retirement worker owns the native
handle, waits for zero, and performs the two-phase sequence. Thus an arbitrary
GC finalizer never calls `dlclose`, blocks on the linker, or runs heavy cleanup.

The final `PROT_NONE` reservation works around a GHC native-object index defect
described in [`safety.md`](safety.md). Physical file mappings and RSS are still
released; only retired 64-bit virtual addresses remain reserved.

## Why the wrapper is necessary

A raw lazy plugin closure cannot be returned safely: its generation counter can
reach zero while a host thunk still contains plugin code or an info-table
pointer. Unloading then turns evaluation into a jump into unmapped memory.

A data shape such as `Managed token value` is also insufficient. A caller can
extract `value` and stop retaining `token`; demand analysis may make this
separation happen without an explicit pattern match. The finalizer can then run
while a plugin thunk remains reachable.

`snapshotFunction` does not expose the pair. Every callable closure is a host
wrapper that needs the token for its eventual terminal `withForeignPtr`, so
partial applications retain both the plugin continuation and its generation.
Terminal `rnf` prevents module-owned result thunks from crossing the point where
that protection ends. Explicit host-owned opaque wrappers are responsible for
their own lawful shallow `NFData` instances.

## Concurrency

- A process-wide lock serializes GHC linker operations and address guards.
- Loading does not wait for in-flight calls; a blocked old call cannot prevent
  the new generation from becoming current.
- Unloading first blocks new native terminal calls, drains committed calls, and
  then performs two-phase retirement.
- A per-runtime lock serializes swap and close.
- Current generations and active counts live in STM. Acquisition and count
  increments are one transaction.
- Retirement runs in its own thread, so cancellation of the swap caller cannot
  abandon cleanup.
- `withForeignPtr` prevents the token finalizer from racing a running managed
  terminal action.

## Minute polling

The core poller is transport-independent. The compiler client supplies its
callback using module and artifact endpoints. The documented default is 60
seconds; tests use short intervals and immutable revisions.
