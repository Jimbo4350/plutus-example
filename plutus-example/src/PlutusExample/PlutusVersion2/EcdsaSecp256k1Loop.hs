{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.PlutusVersion2.EcdsaSecp256k1Loop
    ( v2EcdsaLoopScript
    , v2EcdsaLoopScriptShortBs
    ) where

import           Cardano.Api           (PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusLedgerApi.V2    as PV2
import qualified PlutusTx
import qualified PlutusTx.Builtins     as BI
import           PlutusTx.Prelude      as P hiding (Semigroup (..), unless, (.))
import           Prelude               ((.))

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum red _txContext =
  case PV2.fromBuiltinData red of
    Nothing -> P.traceError "Trace error: Invalid redeemer"
    Just (n, vkey, msg, sig) ->
      if n < (1000000 :: Integer) -- large number ensures same bitsize for all counter values
      then traceError "redeemer is < 1000000"
      else loop n vkey msg sig
  where
    loop i v m s
      | i == 1000000 = ()
      | BI.verifyEcdsaSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: ECDSA validation failed"

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator =  $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV2.serialiseCompiledCode validator

v2EcdsaLoopScriptShortBs :: SBS.ShortByteString
v2EcdsaLoopScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

v2EcdsaLoopScript :: PlutusScript PlutusScriptV2
v2EcdsaLoopScript = PlutusScriptSerialised v2EcdsaLoopScriptShortBs
