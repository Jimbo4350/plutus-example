{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


{-# LANGUAGE ViewPatterns      #-}


module PlutusExample.PlutusVersion2.RedeemerContextEquivalence
  ( PV2CustomRedeemer (..)
  , v2ScriptContextEquivalenceScript
  , v2ScriptContextEquivalenceSbs
  , v2mintEquivScript
  , v2mintEquivScriptShortBs
  ) where

import           Prelude               hiding (($))

import           Cardano.Api.Shelley
import           Prelude               hiding (($), (&&))

import qualified Data.ByteString.Short as SBS


import           PlutusLedgerApi.V2    as PV2
import qualified PlutusTx
import           PlutusTx.Prelude      as PlutusPrelude hiding (Semigroup (..),
                                                         unless, (.))
import qualified PlutusTx.Prelude      as PlutusTx

newtype MyCustomDatumV2 = MyCustomDatumV2 Integer

data PV2CustomRedeemer
  = PV2CustomRedeemer
      { pv2Inputs        :: [PV2.TxInInfo]
      , pv2RefInputs     :: [PV2.TxInInfo]
      , pv2Outputs       :: [PV2.TxOut]
      , pv2Fee           :: PV2.Value
      , pv2Mint          :: PV2.Value
      , pv2DCert         :: [PV2.DCert]
      , pv2Wdrl          :: PV2.Map PV2.StakingCredential Integer
      , pv2ValidRange    :: PV2.POSIXTimeRange
      , pv2Signatories   :: [PV2.PubKeyHash]
      , pv2Redeemers     :: PV2.Map ScriptPurpose PV2.Redeemer
      , pv2Data          :: PV2.Map PV2.DatumHash PV2.Datum
      , pv2ScriptPurpose :: Maybe PV2.ScriptPurpose
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''MyCustomDatumV2
PlutusTx.unstableMakeIsData ''PV2CustomRedeemer

-- @(PV2CustomRedeemer inputs refInputs outputs fee mint dCert wdrl validRange signatories redeemers data)

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatumV2 -> PV2CustomRedeemer -> PV2.ScriptContext -> Bool
mkValidator _ redeemer scriptContext =
  -- These all work fine
  inputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  referenceInputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  certsAreEquivalent redeemer txInfo PlutusPrelude.&&
  reqSignersAreEquivalent redeemer txInfo PlutusPrelude.&&
  datumHashMapsAreEquivalent redeemer txInfo PlutusPrelude.&&
  outputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  correctNumberOfRedeemers redeemer txInfo
  -- These below are failing
  -- validtyIntervalsAreEquivalent redeemer txInfo
  -- Inequality for validity interval doesnt work. Also the interval reported by the script context is a little ahead of
  -- what is in the transaction
  -- TODO: You can't check the fee with the build command due to how it's constructed
  -- These below have not been tested
  -- withdrawalsAreEquivalent redeemer txInfo
 where
  txInfo :: PV2.TxInfo
  txInfo = PV2.scriptContextTxInfo scriptContext

  inputsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  inputsAreEquivalent (PV2CustomRedeemer inputs _ _ _ _ _ _ _ _ _ _ _) tInfo =
    (PlutusPrelude.map txInInfoResolved $ PV2.txInfoInputs tInfo) PlutusPrelude.==
    PlutusPrelude.map txInInfoResolved inputs

  referenceInputsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  referenceInputsAreEquivalent (PV2CustomRedeemer _ refInputs _ _ _ _ _ _ _ _ _ _) tInfo =
    (PlutusPrelude.map txInInfoResolved $ PV2.txInfoReferenceInputs tInfo) PlutusPrelude.==
    PlutusPrelude.map txInInfoResolved refInputs

  outputsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  outputsAreEquivalent (PV2CustomRedeemer _ _ outputs _ _ _ _ _ _ _ _ _) tInfo =
    let scOuts = PV2.txInfoOutputs tInfo
        scOutAddrs = PlutusPrelude.map PV2.txOutAddress scOuts
        scOutValue = PlutusPrelude.map PV2.txOutValue scOuts
        scOutDatums = PlutusPrelude.map PV2.txOutDatum scOuts
        scOutReferenceScripts = PlutusPrelude.map PV2.txOutReferenceScript scOuts

        redeemerOutAddrs = PlutusPrelude.map PV2.txOutAddress outputs
        redeemerOutValue = PlutusPrelude.map PV2.txOutValue outputs
        redeemerOutDatums = PlutusPrelude.map PV2.txOutDatum outputs
        redeemerOutReferenceScripts = PlutusPrelude.map PV2.txOutReferenceScript outputs
    in (scOutAddrs PlutusPrelude.== redeemerOutAddrs) PlutusPrelude.&&
       (scOutDatums PlutusPrelude.== redeemerOutDatums) PlutusPrelude.&&
       (scOutReferenceScripts PlutusPrelude.== redeemerOutReferenceScripts) PlutusPrelude.&&
       -- We want to see if out tx out specified in our tx is equal to one of the txouts in the
       -- script context. So we have a total of 4 outputs when we combine the outputs in the script
       -- context and the redeemer. This would be the two "normal" outputs and the two "change outputs"
       (PlutusPrelude.length (scOutValue PlutusPrelude.++ redeemerOutValue) PlutusPrelude.== 4) PlutusPrelude.&&
       -- You would expect calling nub on the combined values, we should expect a length of 2. However
       -- the change outputs will be different because of how we construct the redeemer. Essentially we
       -- use an idential tx to generate our redeemer (and the redeemer in this tx is a default redeemer with nothing in it)
       -- and then we add that redeemer to a new transaction built with the `build` command. The problem is
       -- the fee and the change outputs created from the initial tx will be different because the size of
       -- the total tx is now different. Therefore we expect the length to be 3 since only the "normal"
       -- txouts are equivalent but the change outputs are different!
       (PlutusPrelude.length (nub $ scOutValue PlutusPrelude.++ redeemerOutValue) PlutusPrelude.== 3)

  certsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  certsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ certs _ _ _ _ _ _) tInfo =
    PV2.txInfoDCert tInfo PlutusPrelude.== certs

  --validtyIntervalsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  --validtyIntervalsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ validInterval _ _ _) tInfo =
  --   (PV2.txInfoValidRange tInfo) PlutusPrelude./= validInterval
    -- PV2.ivFrom (PV2.txInfoValidRange tInfo) PlutusPrelude.== PV2.ivFrom validInterval Fails

  reqSignersAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  reqSignersAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ _ reqSigners _ _ _) tInfo =
    PV2.txInfoSignatories tInfo PlutusPrelude.== reqSigners

  datumHashMapsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  datumHashMapsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ _ datumHashMap _) tInfo =
    PV2.txInfoData tInfo PlutusPrelude.== datumHashMap

  correctNumberOfRedeemers :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  correctNumberOfRedeemers (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ redeemers _ _) tInfo =
    PlutusPrelude.length (PV2.txInfoRedeemers tInfo) PlutusPrelude.== PlutusPrelude.length redeemers

  -- TODO: not done yet
  --withdrawalsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
  --withdrawalsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ wdrwls _ _ _ _) tInfo =
  -- PV2.txInfoWdrl tInfo PlutusPrelude.== wdrwls
  -- TODO: Also need to do separate minting script

typedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
typedValidator typedDatum typedRedeemr ctx =
  PlutusTx.check
    $ mkValidator
        (PlutusTx.unsafeFromBuiltinData typedDatum)
        (PlutusTx.unsafeFromBuiltinData typedRedeemr)
        (PlutusTx.unsafeFromBuiltinData ctx)

compiledTypedValidator :: PlutusTx.CompiledCode   (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledTypedValidator = $$(PlutusTx.compile [|| typedValidator ||])

v2ScriptContextEquivalenceSbs :: SBS.ShortByteString
v2ScriptContextEquivalenceSbs = PV2.serialiseCompiledCode compiledTypedValidator

v2ScriptContextEquivalenceScript :: PlutusScript PlutusScriptV2
v2ScriptContextEquivalenceScript = PlutusScriptSerialised v2ScriptContextEquivalenceSbs

-- Mint field and script purpose equivalence equivalence

{-# INLINABLE mkMintEquivalenceValidator #-}
mkMintEquivalenceValidator :: PV2CustomRedeemer -> PV2.ScriptContext -> Bool
mkMintEquivalenceValidator redeemer scriptContext =
  -- Minted value is equivalent
  mintingFieldsAreEquivalent redeemer txInfo PlutusPrelude.&&
  -- Script purpose is equivalent
  scriptPurposeIsEquivalent scriptContext redeemer
 where
   txInfo :: PV2.TxInfo
   txInfo = PV2.scriptContextTxInfo scriptContext

   mintingFieldsAreEquivalent :: PV2CustomRedeemer -> PV2.TxInfo -> Bool
   mintingFieldsAreEquivalent (PV2CustomRedeemer _ _ _ _ mint _ _ _ _ _ _ _) tInfo =
    PV2.txInfoMint tInfo  PlutusPrelude.== mint

   scriptPurposeIsEquivalent :: PV2.ScriptContext -> PV2CustomRedeemer -> Bool
   scriptPurposeIsEquivalent sc (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ _ _ mScPurpose) =
    case mScPurpose of
      Just sPurp -> PV2.scriptContextPurpose sc PlutusPrelude.== sPurp
      Nothing    -> PlutusPrelude.error ()

typedMintEquivalenceValidator :: BuiltinData -> BuiltinData -> ()
typedMintEquivalenceValidator typedRedeemer ctx =
  PlutusTx.check
    $  mkMintEquivalenceValidator
         (PlutusTx.unsafeFromBuiltinData typedRedeemer)
         (PlutusTx.unsafeFromBuiltinData ctx)

compiledtypedMintEquivalenceValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
compiledtypedMintEquivalenceValidator = $$(PlutusTx.compile [|| typedMintEquivalenceValidator ||])

v2mintEquivScript :: PlutusScript PlutusScriptV2
v2mintEquivScript = PlutusScriptSerialised v2mintEquivScriptShortBs

v2mintEquivScriptShortBs :: SBS.ShortByteString
v2mintEquivScriptShortBs = PV2.serialiseCompiledCode compiledtypedMintEquivalenceValidator
