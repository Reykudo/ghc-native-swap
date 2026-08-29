# ghc-native-swap

`ghc-native-swap` compiles and loads versioned, Haskell-only native shared objects into
one running RTS. Calls cross generations as ordinary Haskell values; there is
no RPC or runtime serialization layer.

Results cross the unload boundary through the `UnloadSafe` contract. Lawful
`NFData` values are supported automatically, and host-created
`Data.Dynamic.Dynamic` values may be transported opaquely.

Project documentation starts at [`docs/README.md`](docs/README.md).
