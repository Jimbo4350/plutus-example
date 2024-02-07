{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion2.StakeScript
  ( v2StakeScript
  , v2StakeScriptShortBs
  ) where

import           Prelude               hiding (($))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV2)
import           Prelude               hiding (($), (&&))

import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V2    as PV2
import qualified PlutusTx

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PV2.ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> ScriptContext -> Bool)
compiledValidator = $$(PlutusTx.compile [|| mkPolicy ||])

plutusScript :: SerialisedScript
plutusScript = PV2.serialiseCompiledCode compiledValidator

v2StakeScript :: PlutusScript PlutusScriptV2
v2StakeScript = PlutusScriptSerialised plutusScript

v2StakeScriptShortBs :: SBS.ShortByteString
v2StakeScriptShortBs = plutusScript
