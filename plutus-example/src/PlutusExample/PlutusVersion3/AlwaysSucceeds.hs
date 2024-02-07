{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion3.AlwaysSucceeds
  ( alwaysSucceedsScript
  , alwaysSucceedsScriptShortBs
  ) where

import           Prelude               hiding (($))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV3)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V3    as PV3
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: PV3.BuiltinData -> PV3.BuiltinData -> PV3.BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: PlutusTx.CompiledCode   (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkValidator ||])

script :: PV3.SerialisedScript
script = PV3.serialiseCompiledCode validator

alwaysSucceedsScriptShortBs :: SBS.ShortByteString
alwaysSucceedsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysSucceedsScript :: PlutusScript PlutusScriptV3
alwaysSucceedsScript = PlutusScriptSerialised alwaysSucceedsScriptShortBs

