{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.PlutusVersion1.DatumRedeemerGuess
  ( guessScript
  , guessScriptStake
  , datumRedeemerGuessScriptShortBs
  ) where

import           Prelude               hiding (($), (&&), (==))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusLedgerApi.V1    as PV1

import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData  -> ()
mkValidator datum redeemer _txContext
  |    datum    == PlutusTx.toBuiltinData (42 :: Integer)
    && redeemer == PlutusTx.toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."


validator :: PlutusTx.CompiledCode   (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode validator

datumRedeemerGuessScriptShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

guessScript :: PlutusScript PlutusScriptV1
guessScript = PlutusScriptSerialised datumRedeemerGuessScriptShortBs

{-# INLINEABLE mkValidatorStake #-}
mkValidatorStake :: BuiltinData -> BuiltinData -> ()
mkValidatorStake redeemer _txContext
  | redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

validatorStake :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
validatorStake = $$(PlutusTx.compile [||mkValidatorStake||])

scriptStake :: SerialisedScript
scriptStake = PV1.serialiseCompiledCode validatorStake

datumRedeemerGuessScriptStakeShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptStakeShortBs = SBS.toShort . LBS.toStrict $ serialise scriptStake

guessScriptStake :: PlutusScript PlutusScriptV1
guessScriptStake = PlutusScriptSerialised datumRedeemerGuessScriptStakeShortBs
