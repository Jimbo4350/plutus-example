{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.PlutusVersion2.SchnorrSecp256k1Loop
    ( v2SchnorrLoopScript
    , v2SchnorrLoopScriptShortBs
    ) where

import           Cardano.Api           (PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley   (PlutusScript (..))
import qualified Data.ByteString.Short as SBS
import           PlutusLedgerApi.V2    as PV2
import qualified PlutusTx
import qualified PlutusTx.Builtins     as BI
import           PlutusTx.Prelude      as P hiding (Semigroup (..), unless, (.))

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
      | BI.verifySchnorrSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: Schnorr validation failed"

compiledValidator :: PlutusTx.CompiledCode   (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledValidator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV2.serialiseCompiledCode compiledValidator

v2SchnorrLoopScriptShortBs :: SBS.ShortByteString
v2SchnorrLoopScriptShortBs = script

v2SchnorrLoopScript :: PlutusScript PlutusScriptV2
v2SchnorrLoopScript = PlutusScriptSerialised v2SchnorrLoopScriptShortBs
