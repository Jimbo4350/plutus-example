{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion1.AlwaysFails
  ( alwaysFailsScript
  , alwaysFailsScriptShortBs
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
mkValidator _ _ _ = PlutusTx.Prelude.error ()

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode validator

alwaysFailsScriptShortBs :: SBS.ShortByteString
alwaysFailsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysFailsScript :: PlutusScript PlutusScriptV1
alwaysFailsScript = PlutusScriptSerialised alwaysFailsScriptShortBs

