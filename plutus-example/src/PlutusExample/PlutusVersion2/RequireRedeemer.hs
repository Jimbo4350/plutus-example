{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion2.RequireRedeemer
  ( requireRedeemerScript
  , requireRedeemerScriptShortBs
  ) where

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV2)
import           Prelude               hiding (($), (&&))

import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V2    as PV2

import qualified PlutusTx
import           PlutusTx.Builtins
import           PlutusTx.Eq           as PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))
import qualified PlutusTx.Prelude      as PlutusPrelude

-- serialiseData is a PlutusV2 builtin

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> PV2.ScriptContext -> Bool
mkValidator _ redeemer sc =
  serialiseData redeemer PlutusTx./= emptyByteString &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== PV2.OutputDatum (PV2.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    txinsDatums) &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== PV2.OutputDatum (PV2.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    referenceInputDatums)
 where
  txInfo = PV2.scriptContextTxInfo sc
  txinsDatums = PlutusPrelude.map (txOutDatum . txInInfoResolved)
                  $ PV2.txInfoInputs txInfo
  referenceInputDatums =
    PlutusPrelude.map (txOutDatum . txInInfoResolved)
      $ PV2.txInfoReferenceInputs txInfo

{-# INLINABLE compiledValidator #-}
compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ScriptContext -> Bool)
compiledValidator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV2.serialiseCompiledCode compiledValidator

requireRedeemerScriptShortBs :: SBS.ShortByteString
requireRedeemerScriptShortBs = script

requireRedeemerScript :: PlutusScript PlutusScriptV2
requireRedeemerScript = PlutusScriptSerialised requireRedeemerScriptShortBs
