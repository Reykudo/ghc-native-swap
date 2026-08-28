# Testing strategy

## Layers

1. **Pure tests** cover public slot/module validation and protocol behaviour.
2. **Compiler tests** build real modules, run the ABI probe, publish through
   HTTP, download immutable bytes, and preserve the last good revision.
3. **Runtime subprocess tests** load real `.so` files, invoke them, swap while
   an old call is active, cancel callers, propagate plugin failures, reject bad
   magic/types/symbols, poll revisions, close, and inspect mappings.
4. **Managed lifetime tests** cover zero arguments, multiple arguments, a
   reachable partial application, terminal `rnf`, IORef replacement, a running
   call during finalization, and sequential function shapes.
5. **Direct concurrency** executes 160,000 calls through one generation to
   isolate shared-heap invocation from unloading.
6. **Unload stress** compiles unique artifacts and runs three child processes:
   strict concurrent swapping, managed concurrent snapshot replacement, and up
   to 64 complete sequential managed load/finalize/close lifecycles.

## Crash containment

Every test that reaches the native loader runs in a child invocation of the
test executable. The parent enforces a timeout and requires `ExitSuccess`.
`SIGSEGV`, `SIGABRT`, glibc double frees, RTS panics, deadlocks, and ordinary
assertion failures therefore become reproducible test failures instead of
killing the full runner.

Invalid artifacts are Haskell-only shared objects built separately and never
installed as current unless exact typed-symbol lookup succeeds.

## Laziness gates

The managed suite explicitly proves:

- `Dynamic (IO Int)` works without inventing a dummy `()` argument;
- `Dynamic (Int -> Int -> IO Int)` works through recursive wrappers;
- retaining only `function firstArgument` blocks close and remains callable;
- dropping that partial application and running a major GC permits unload;
- a latent exception in a list tail is raised before managed protection ends;
- replacing an `IORef` snapshot retires only the no-longer-reachable generation;
- a token finalizer cannot retire a terminal call that is still running.
- retired file mappings disappear and each former interval is covered by an
  anonymous `---p` tombstone.

These tests exist because a smoke call cannot distinguish a safe wrapper from a
token that the optimiser may finalize independently of a plugin thunk.

## Native index regression

Two sequential finalizer-managed lifecycles originally reproduced stock GHC
9.10.3 corruption:

```text
debug RTS: ASSERTION FAILED: rts/CheckUnload.c, line 489
normal RTS: free(): double free detected in tcache 2
```

The failure occurs when a new `.so` reuses a retired address whose stale
`OCSectionIndex` still points at freed metadata. The sequential managed tests
exercise the address tombstones directly; the focused two-shape reproducer was
also run with the debug RTS while developing the fix.

## Acceptance gates

Before release:

```text
cabal build all
cabal test all --test-show-details=direct
GHC_NATIVE_SWAP_STRESS_GENERATIONS=1000 \
  cabal test ghc-native-swap-test --test-show-details=direct \
  --test-options='--pattern "concurrent stress reload"'
```

The 1,000-generation gate uses:

- eight capabilities and eight callers active during strict and managed swaps;
- recursive host wrappers with snapshot replacement and repeated major GC;
- 64 additional full sequential managed lifecycles;
- unique immutable paths and type-specific isolated unit IDs;
- real GHC compilation and ABI probes;
- forced collections before and after every unload request;
- `/proc/self/maps` assertions for every retired file path.

A compiler handshake, successful `dlopen`, or one smoke invocation is not a
safety result.

## Longer validation

Release candidates should repeat the 1,000-generation gate and run an
hours-long soak while recording RSS, reserved `PROT_NONE` address space,
stable-pointer activity, swap latency, and mapping count. A new GHC patch
release is unsupported until this complete matrix passes again.

## Verified evidence

Historical strict-only baseline on 2026-08-23, Linux x86-64, GHC 9.10.3:

- all 15 then-current tests passed in 64.84 seconds;
- 1,000 strict compile/swap/unload generations passed with eight callers in
  592.35 seconds of test time;
- every retired artifact path disappeared from `/proc/self/maps`.

Final managed acceptance on 2026-08-24, same platform:

- `cabal build all` passes;
- `cabal test all --test-show-details=direct` passes all 24 tests in 70.46
  seconds;
- the formerly crashing two-shape lifecycle passes under the debug RTS without
  artificial delays;
- `GHC_NATIVE_SWAP_STRESS_GENERATIONS=1000 cabal test ghc-native-swap-test
  --test-show-details=direct --test-options='--pattern "concurrent stress
  reload"'` passes in 584.53 seconds (588.89 seconds including setup):
  strict concurrent swapping, managed snapshot replacement with eight callers,
  and 64 full sequential managed lifecycles;
- every retired file path disappears from `/proc/self/maps`, and focused
  managed tests assert anonymous `---p` coverage of every retired interval.
