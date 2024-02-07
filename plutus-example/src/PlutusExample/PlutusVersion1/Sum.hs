{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.PlutusVersion1.Sum
  where

import           Prelude               hiding (($), (+), (-), (==))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V1    as PV1
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))
import qualified PlutusTx.Prelude      as PlutusTx


smartSum :: Integer -> Integer
smartSum a = loop a 0
 where
  loop !n !acc = if n==0
    then acc
    else loop (n - 1) (n + acc)

-- | The validation function
{-# INLINABLE validateSum #-}
validateSum :: Integer -> Integer -> ScriptContext -> Bool
validateSum n s _ = isGoodSum n s

{-# INLINABLE isGoodSum #-}
isGoodSum :: Integer -> Integer -> Bool
isGoodSum n s = smartSum n == s

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator typedDatum typedRedeemer ctx =
  PlutusTx.check
     $ validateSum
         (PlutusTx.unsafeFromBuiltinData typedDatum)
         (PlutusTx.unsafeFromBuiltinData typedRedeemer)
         (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINEABLE validator #-}
validator :: PlutusTx.CompiledCode   (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator =  $$(PlutusTx.compile [|| untypedValidator ||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode validator

sumScriptShortBs :: SBS.ShortByteString
sumScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

sumScript :: PlutusScript PlutusScriptV1
sumScript = PlutusScriptSerialised sumScriptShortBs
