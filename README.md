# ghc-native-swap

`ghc-native-swap` compiles and loads versioned, Haskell-only native shared objects into
one running RTS. Calls cross generations as ordinary Haskell values; there is
no RPC or runtime serialization layer.

Results cross the unload boundary through their standard `NFData` instances.
Function results are rejected despite deepseq's deprecated shallow function
instance. Host API packages may define explicit shallow `NFData` newtypes for
opaque, permanently owned values such as `Data.Dynamic.Dynamic`.

Project documentation starts at [`docs/README.md`](docs/README.md).
