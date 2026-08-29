{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Values that may outlive an unloaded native generation.

The instance law is stronger than normal-form evaluation: after
'rnfUnloadSafe' returns, the value must not retain code, info tables, static
data, finalizers, or closures owned by the reloadable module.
-}
module GHC.NativeSwap.UnloadSafe
  ( UnloadSafe (..)
  , forceUnloadSafe
  ) where

import Control.DeepSeq (NFData, rnf)
import Data.Dynamic qualified as Haskell
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.TypeError (ErrorMessage (..), TypeError)

-- | A trusted proof that a value may outlive a reloadable native generation.
class UnloadSafe value where
  rnfUnloadSafe :: value -> ()

-- | Keep the value after checking the unload-safety law.
forceUnloadSafe :: (UnloadSafe value) => value -> value
forceUnloadSafe value = rnfUnloadSafe value `seq` value

{- | A lawful 'NFData' instance normally proves the stronger property. This
instance is deliberately overlappable so structural instances below can keep
host-owned opaque leaves without pretending that their payload is in normal
form.
-}
instance {-# OVERLAPPABLE #-} (NFData value) => UnloadSafe value where
  rnfUnloadSafe = rnf

{- | The payload is not forced. This instance is lawful only for a @Dynamic@
whose payload and @TypeRep@ were created by permanently loaded host code. A
reloadable module must only transport such a value; constructing its own
@Dynamic@ violates the class law.
-}
instance {-# OVERLAPPING #-} UnloadSafe Haskell.Dynamic where
  rnfUnloadSafe value = value `seq` ()

instance {-# OVERLAPPING #-} (UnloadSafe value) => UnloadSafe [value] where
  rnfUnloadSafe = foldr (\value rest -> rnfUnloadSafe value `seq` rest) ()

instance {-# OVERLAPPING #-} (UnloadSafe value) => UnloadSafe (Maybe value) where
  rnfUnloadSafe Nothing = ()
  rnfUnloadSafe (Just value) = rnfUnloadSafe value

instance {-# OVERLAPPING #-} (UnloadSafe left, UnloadSafe right) => UnloadSafe (Either left right) where
  rnfUnloadSafe (Left value) = rnfUnloadSafe value
  rnfUnloadSafe (Right value) = rnfUnloadSafe value

instance {-# OVERLAPPING #-} (UnloadSafe first, UnloadSafe second) => UnloadSafe (first, second) where
  rnfUnloadSafe (first, second) =
    rnfUnloadSafe first `seq` rnfUnloadSafe second

instance {-# OVERLAPPING #-} (UnloadSafe first, UnloadSafe second, UnloadSafe third) => UnloadSafe (first, second, third) where
  rnfUnloadSafe (first, second, third) =
    rnfUnloadSafe first `seq`
      rnfUnloadSafe second `seq`
        rnfUnloadSafe third

instance {-# OVERLAPPING #-} (UnloadSafe key, UnloadSafe value) => UnloadSafe (Map key value) where
  rnfUnloadSafe =
    Map.foldrWithKey
      (\key value rest -> rnfUnloadSafe key `seq` rnfUnloadSafe value `seq` rest)
      ()

instance {-# OVERLAPPING #-} (UnloadSafe value) => UnloadSafe (Set value) where
  rnfUnloadSafe = Set.foldr (\value rest -> rnfUnloadSafe value `seq` rest) ()

instance {-# OVERLAPPING #-} (UnloadSafe value) => UnloadSafe (Vector value) where
  rnfUnloadSafe = Vector.foldr (\value rest -> rnfUnloadSafe value `seq` rest) ()

instance
  {-# OVERLAPPING #-}
  ( TypeError
      ( 'Text "A function result is not UnloadSafe"
          ':$$: 'Text "Return a fully detached host value or keep the generation pinned"
      )
  )
  => UnloadSafe (argument -> result)
  where
  rnfUnloadSafe _ = ()
