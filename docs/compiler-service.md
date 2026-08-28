# Compiler service

## Source contract

The submitted module contains an ordinary Haskell binding named `invoke`; users
do not write ABI declarations, generated wrappers, or `foreign export` stubs.
The strict convenience shape is:

```haskell
module Company.Rules.Plugin where

import GHC.NativeSwap.Plugin (Entry)

invoke :: Entry Request Response
invoke = applyRules
```

The same service accepts zero or multiple ordinary arguments when the type ends
in `IO`:

```haskell
invoke :: IO Result

invoke :: Int -> Text -> IO Result
```

The module name in JSON must match the source declaration. The configured
package allowlist must include `base`, `ghc-native-swap`, and every shared API package
used by request, response, or implementation code. `GHCNativeSwapGenerated` is
reserved for the compiler-created ABI module.

The service first builds an executable probe importing this exact binding. A
missing binding, ambiguous type, unavailable `Typeable` representation, or
other source error is a normal compilation failure. The probe produces the
actual whole-binding v4 fingerprint. The service then generates a normal
`Export Plugin.invoke` Haskell binding with that fingerprint in its name. No C
source or Haskell `foreign export` is generated.

## HTTP API

### Health

```http
GET /healthz
```

Returns `200` with status, full GHC version, target platform, and ABI version.

### Compile and publish

```http
PUT /v1/modules/{slot}
Content-Type: application/json

{
  "moduleName": "Company.Rules.Plugin",
  "source": "module Company.Rules.Plugin where ..."
}
```

Compilation and probing happen in an isolated staging directory. A successful
`.so` is moved to a new immutable path before its manifest becomes the latest
revision. A failed probe, compile, link, or move never replaces the last good
manifest.

Success returns `201`. Invalid input returns `400`, compiler diagnostics return
`422`, and a timed-out compiler/probe process returns `504`.

### Latest manifest

```http
GET /v1/modules/{slot}
```

Returns the current immutable manifest or `404` when nothing was published.

### Artifact bytes

```http
GET /v1/artifacts/{artifactId}
```

Returns `application/x-sharedlib`. Artifact IDs and slots contain only ASCII
letters, digits, `_`, and `-`.

## Configuration

| Variable | Default | Meaning |
| --- | --- | --- |
| `GHC_NATIVE_SWAP_BIND` | `127.0.0.1` | Listen address |
| `GHC_NATIVE_SWAP_PORT` | `8080` | Listen port |
| `GHC_NATIVE_SWAP_ARTIFACT_DIR` | `./artifacts` | Artifact and staging root |
| `GHC_NATIVE_SWAP_GHC` | `ghc` | Exact compiler executable |
| `GHC_NATIVE_SWAP_PACKAGE_DBS` | empty | Comma-separated package DB directories |
| `GHC_NATIVE_SWAP_PACKAGES` | `base,ghc-native-swap` | Exposed package allowlist |
| `GHC_NATIVE_SWAP_COMPILE_TIMEOUT_SECONDS` | `60` | Timeout for each compiler/probe process |
| `GHC_NATIVE_SWAP_MAX_SOURCE_BYTES` | `1048576` | HTTP request body limit |
| `GHC_NATIVE_SWAP_MAX_CONCURRENT_COMPILATIONS` | `1` | Concurrent request limit |

Clients cannot supply GHC flags, output paths, package DBs, or package names.
The service chooses a fresh immutable artifact path, derives a reserved
type-specific `hotswapplugin...` unit ID, and emits all dynamic-linking flags and
the Haskell ABI module itself. `clang` must be available on `PATH`; GHC uses its
integrated assembler because GNU `as` is prohibitively slow for very large
generated modules.

## Security boundary

Compiling and running a Haskell ABI probe is arbitrary code execution. Source
filters are not a sandbox. Run the service as a dedicated unprivileged user in
a container or VM with CPU, memory, process, filesystem, and network limits; a
read-only compiler/package store; and a disposable artifact volume. Never
expose it directly to the Internet.

Authentication, authorization, durable manifests, signing, digest verification,
and remote object storage are deployment responsibilities outside the compact
core.
