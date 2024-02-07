{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.PlutusVersion1.Loop
  ( loopScript
  , loopScriptShortBs
  ) where

import           Prelude               hiding (pred, ($), (&&), (<), (==))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           PlutusLedgerApi.V1    as PV1
import           PlutusTx
import           PlutusTx.Builtins     (unsafeDataAsI)
import           PlutusTx.Prelude      hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum redeemer _txContext
  = if n < 1000000
       then traceError "redeemer is < 1000000"
       else loop n
  where
    n = unsafeDataAsI redeemer
    loop i = if i == 1000000 then () else loop $ pred i

validator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkValidator ||])

script :: SerialisedScript
script = PV1.serialiseCompiledCode validator

loopScriptShortBs :: SBS.ShortByteString
loopScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

loopScript :: PlutusScript PlutusScriptV1
loopScript = PlutusScriptSerialised loopScriptShortBs
