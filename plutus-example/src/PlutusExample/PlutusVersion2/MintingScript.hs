{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion2.MintingScript
  ( v2mintingScript
  , v2mintingScriptShortBs
  ) where

import           Prelude             hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import           Prelude             hiding (($), (&&))

import           PlutusLedgerApi.V2  as PV2
import qualified PlutusTx
{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PV2.ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: PlutusTx.CompiledCode (BuiltinData -> ScriptContext -> Bool)
policy = $$(PlutusTx.compile [|| mkPolicy ||])

v2mintingScriptShortBs :: SerialisedScript
v2mintingScriptShortBs = PV2.serialiseCompiledCode policy

v2mintingScript :: PlutusScript PlutusScriptV2
v2mintingScript = PlutusScriptSerialised v2mintingScriptShortBs

