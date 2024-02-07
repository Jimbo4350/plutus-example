{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-# LANGUAGE ViewPatterns          #-}


module PlutusExample.PlutusVersion1.CustomDatumRedeemerGuess
  ( MyCustomDatum(..)
  , MyCustomRedeemer(..)
  , customGuessScript
  , customDatumRedeemerGuessScriptAsShortBs
  ) where

import           Prelude               hiding (($), (&&), (==))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V1    as PV1
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup ((<>)), unless, (.))
import qualified PlutusTx.Prelude      as PlutusTx

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer Integer

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

{-# INLINABLE typedValidator #-}
typedValidator :: MyCustomDatum -> MyCustomRedeemer -> ScriptContext -> Bool
typedValidator (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator typedDatum typedRedeemer ctx =
  PlutusTx.check
     $ typedValidator
         (PlutusTx.unsafeFromBuiltinData typedDatum)
         (PlutusTx.unsafeFromBuiltinData typedRedeemer)
         (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINEABLE compiledValidator #-}
compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledValidator = $$(PlutusTx.compile [||untypedValidator||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode compiledValidator

customDatumRedeemerGuessScriptAsShortBs :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

customGuessScript :: PlutusScript PlutusScriptV1
customGuessScript = PlutusScriptSerialised customDatumRedeemerGuessScriptAsShortBs
