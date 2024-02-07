{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion1.AlwaysSucceeds
  ( alwaysSucceedsScript
  , alwaysSucceedsScriptShortBs
  ) where

import           Prelude               hiding (($))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V1    as PV1
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: PlutusTx.CompiledCode  (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode validator

alwaysSucceedsScriptShortBs :: SBS.ShortByteString
alwaysSucceedsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysSucceedsScript :: PlutusScript PlutusScriptV1
alwaysSucceedsScript = PlutusScriptSerialised alwaysSucceedsScriptShortBs

