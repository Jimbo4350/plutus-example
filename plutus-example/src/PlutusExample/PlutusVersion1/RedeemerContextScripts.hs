{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

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


module PlutusExample.PlutusVersion1.RedeemerContextScripts
  ( PV1CustomRedeemer(..)
  , compiledValidator
  -- , pv1RedeemerContextTestScriptBs
  --, pv1CustomRedeemerFromScriptData
 -- , scriptContextTestMintingScript
  , scriptContextTextPayingScript
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified PlutusLedgerApi.Common as PV1
import           PlutusLedgerApi.V1 as Plutus

import           Prelude hiding (($))

import qualified PlutusTx
import qualified PlutusTx.AssocMap as AMap
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Prelude as PlutusTx

-- Description
-- PV1CustomRedeemer mimics the ScriptContext. PV1CustomRedeemer is built via reading
-- the transaction containing the script and the script itself just compares PV1CustomRedeemer
-- to the ScriptContext to be sure they are equivalent.
-- The overall aim is to make sure what is provided via ScriptContext (i.e. the transaction)
-- is what it's supposed to be. We check this by creating PV1CustomRedeemer based on
-- the actual transaction which is created via the create-script-context executable.


newtype MyCustomDatum = MyCustomDatum Integer

data PV1CustomRedeemer
  = PV1CustomRedeemer
      { mCrOutputs       :: [Plutus.TxOut]
      , mCrInputs        :: [Plutus.TxInInfo]
      , mCrMint          :: Plutus.Value
      , mCrValidRange    :: Plutus.POSIXTimeRange
      , mCrFee           :: Plutus.Value
      , mCrDatums        :: [(Plutus.DatumHash, Plutus.Datum)]
      , mCrCerts         :: [Plutus.DCert]
      , mCrSignatories   :: [Plutus.PubKeyHash]
      , mCrScriptPurpose :: Maybe Plutus.ScriptPurpose
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''PV1CustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> PV1CustomRedeemer -> Plutus.ScriptContext -> Bool
mkValidator _datum (PV1CustomRedeemer txouts txins minted txValidRange _fee datumsAndHashes certs signatories mPurpose) scriptContext =
  -- Minted field is equivalent
  Plutus.txInfoMint txInfo P.== minted P.&&
  -- Validity range is equivalent
  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Datums and datum hashes are equivalent
  Plutus.txInfoData txInfo P.== datumsAndHashes P.&&
  -- Required tx signers are equivalent
  Plutus.txInfoSignatories txInfo P.== signatories P.&&
  -- Payment tx out is equivalent
  AMap.member paymentOutputFromRedeemer scriptContextOutputsMap P.&&
  -- Txins are equivalent
  (AMap.member txinA (PlutusTx.error () ) P.&& AMap.member txinB (PlutusTx.error ())) P.&&
  -- Check if tx inputs are equivalent
  AMap.member singleRedeemerCert scriptContextCertsMap P.&&
  -- Check if the script purposes are equivalent
  case mPurpose of
    Just sPurp -> sPurp P.== sPurpose
    Nothing    -> PlutusTx.Prelude.error ()
 where
   scriptContextCertsMap :: AMap.Map Plutus.DCert Integer
   scriptContextCertsMap = AMap.fromList P.$ P.zip (Plutus.txInfoDCert txInfo) [1]

   singleRedeemerCert :: Plutus.DCert
   singleRedeemerCert = P.head certs

   txinA :: Plutus.TxInInfo
   txinA = P.head redeemerTxins

   txinB :: Plutus.TxInInfo
   txinB = P.head $ P.reverse redeemerTxins

   redeemerTxins :: [Plutus.TxInInfo]
   redeemerTxins = txins

   paymentOutputFromRedeemer = P.head $ P.reverse redeemerValues

   redeemerValues :: [Plutus.Value]
   redeemerValues = P.map Plutus.txOutValue txouts

   scriptContextOutputValues :: [Plutus.Value]
   scriptContextOutputValues = P.map Plutus.txOutValue $ Plutus.txInfoOutputs txInfo

   scriptContextOutputsMap :: AMap.Map Plutus.Value Integer
   scriptContextOutputsMap = AMap.fromList P.$ P.zip scriptContextOutputValues [1,2 :: Integer]

   txInfo :: Plutus.TxInfo
   txInfo = Plutus.scriptContextTxInfo scriptContext

   sPurpose :: Plutus.ScriptPurpose
   sPurpose = Plutus.scriptContextPurpose scriptContext

{-# INLINABLE validator #-}
validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator typedDatum typedRedeemer ctx =
  PlutusTx.check
     $ mkValidator
         (PlutusTx.unsafeFromBuiltinData typedDatum)
         (PlutusTx.unsafeFromBuiltinData typedRedeemer)
         (PlutusTx.unsafeFromBuiltinData ctx)


compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledValidator = $$(PlutusTx.compile [|| validator ||])
--
pv1RedeemerContextTestScriptBs :: SerialisedScript
pv1RedeemerContextTestScriptBs = PV1.serialiseCompiledCode compiledValidator

scriptContextTextPayingScript :: PlutusScript PlutusScriptV1
scriptContextTextPayingScript = PlutusScriptSerialised pv1RedeemerContextTestScriptBs


-- Minting script that checks the minting value, validty interval and
-- required signers in the ScriptContext is equivalent to what's in the
-- redeemer.

--{-# INLINABLE mkPolicy #-}
--mkPolicy :: PV1CustomRedeemer -> Plutus.ScriptContext -> Bool
--mkPolicy (PV1CustomRedeemer _ _ minted txValidRange _fee _ _ signatories mPurpose) scriptContext =
--  -- Minted value is equivalent
--  minted P.== Plutus.txInfoMint txInfo P.&&
--  -- Validity range is equivalent
--  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
--  -- Required signers are equivalent
--  AMap.member singleSignatory scriptContextSignatoriesMap P.&&
--
--  case mPurpose of
--    Just sPurp -> sPurp P.== sPurpose
--    Nothing    -> PlutusTx.Prelude.error ()
-- where
--   sPurpose :: Plutus.ScriptPurpose
--   sPurpose = Plutus.scriptContextPurpose scriptContext
--
--   scriptContextSignatoriesMap :: AMap.Map Plutus.PubKeyHash Integer
--   scriptContextSignatoriesMap = AMap.fromList P.$ P.zip (Plutus.txInfoSignatories txInfo) [1]
--
--   singleSignatory :: Plutus.PubKeyHash
--   singleSignatory = P.head signatories
--
--   txInfo :: Plutus.TxInfo
--   txInfo = Plutus.scriptContextTxInfo scriptContext
--
--mintingScriptContextTextPolicy :: PlutusTx.CompiledCode (PV1CustomRedeemer -> ScriptContext -> Bool)
--mintingScriptContextTextPolicy = $$(PlutusTx.compile [|| mkPolicy ||])
--
--plutusV1RedeemerContextTestMintingScript :: SerialisedScript
--plutusV1RedeemerContextTestMintingScript =
--  PV1.serialiseCompiledCode mintingScriptContextTextPolicy
--
--
--
--scriptContextTextMintingScript :: LB.ByteString
--scriptContextTextMintingScript = serialise plutusV1RedeemerContextTestMintingScript
--
--scriptContextTestMintingScript :: PlutusScript PlutusScriptV1
--scriptContextTestMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptContextTextMintingScript
--
---- Helpers
--
pv1CustomRedeemerFromScriptData :: ScriptData -> Either String PV1CustomRedeemer
pv1CustomRedeemerFromScriptData sDat =
  let bIData = PlutusTx.dataToBuiltinData $ toPlutusData sDat
  in case PlutusTx.fromBuiltinData bIData of
      Just mCRedeem -> Right mCRedeem
      Nothing       -> Left "Could not decode PV1CustomRedeemer from ScriptData"

