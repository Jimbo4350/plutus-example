{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion1.MintingScript
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Prelude             hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           PlutusLedgerApi.V1  as PV1

import qualified PlutusTx

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: PlutusTx.CompiledCode (BuiltinData -> ScriptContext -> Bool)
policy = $$(PlutusTx.compile [|| mkPolicy ||])

mintingScriptShortBs :: SerialisedScript
mintingScriptShortBs = PV1.serialiseCompiledCode policy

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised mintingScriptShortBs
