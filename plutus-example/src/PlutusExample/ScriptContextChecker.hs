{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module PlutusExample.ScriptContextChecker where

import           Cardano.Api
import           Cardano.Api.Ledger
import qualified Cardano.Api.Ledger as Legder
import           Cardano.Api.Pretty
import qualified Cardano.Api.Shelley as Api

import           Cardano.CLI.Read
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Plutus.Context as Ledger
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Alonzo.UTxO as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Ledger
import qualified Cardano.Ledger.Babbage.TxInfo as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Cardano.Ledger.Plutus.Language as Ledger
import qualified Cardano.Ledger.Plutus.TxInfo as Ledger
import           Cardano.Ledger.Shelley.Tx ()
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.UTxO as Ledger
import           Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           PlutusLedgerApi.V1 as V1
import qualified PlutusLedgerApi.V1 as PV1
import           PlutusLedgerApi.V2 as V2
import qualified PlutusLedgerApi.V2 as PV2

import           Prelude hiding (($))

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Proxy
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import           Data.Word
import           Lens.Micro

import           PlutusExample.PlutusVersion1.RedeemerContextScripts
import           PlutusExample.PlutusVersion2.RedeemerContextEquivalence
import qualified PlutusTx.AssocMap as PMap
import           PlutusTx.Prelude as PPrelude hiding (Eq, Semigroup (..), unless, (.))

data ACustomRedeemer where
  ACustomRedeemer :: AnyCustomRedeemer era -> ACustomRedeemer

data AnyCustomRedeemer era
  = AnyPV1CustomRedeemer
      (AlonzoEraOnwards era)
      PV1CustomRedeemer
  | AnyPV2CustomRedeemer
      (BabbageEraOnwards era)
      PV2CustomRedeemer

-- We convert our custom redeemer to ScriptData so we can include it
-- in our transaction.
customRedeemerToScriptData
  :: AnyCustomRedeemer era
  -> Ledger.Data (Api.ShelleyLedgerEra era)
customRedeemerToScriptData (AnyPV1CustomRedeemer aEraOnwards cRedeem) =
   case aEraOnwards of
     AlonzoEraOnwardsAlonzo  -> Ledger.Data $ V1.toData cRedeem
     AlonzoEraOnwardsBabbage -> Ledger.Data $ V1.toData cRedeem
     AlonzoEraOnwardsConway  -> Ledger.Data $ V1.toData cRedeem
customRedeemerToScriptData  (AnyPV2CustomRedeemer aEraOnwards cRedeem) =
    case aEraOnwards of
      BabbageEraOnwardsBabbage -> Ledger.Data $ V2.toData cRedeem
      BabbageEraOnwardsConway  -> Prelude.error "TODO: Conway"

data ScriptContextError era
  = ReadTxBodyError CddlError
  | DCertRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | TxInRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | TxOutRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | TxOutRelatedAlonzo Text
  | CertificateRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | ScriptPurposeRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | RedeemerRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | QueryFailure UnsupportedNtcVersionError
  | QueryFailureEra EraMismatch
  | QueryAcquiringFailure Api.AcquiringFailure
  | NoScriptsInByronEra
  | MoreThanOneTxInput
  | MoreThanOneScriptPurpose [(Ledger.PlutusPurpose Ledger.AsItem (Api.ShelleyLedgerEra era), Ledger.ScriptHash (EraCrypto (Api.ShelleyLedgerEra era)))]
  | IntervalConvError Text
  | AcquireFail Api.AcquiringFailure
  | NoSystemStartTimeError
  | EraMismatch !Consensus.EraMismatch

instance Ledger.Inject (Ledger.AlonzoContextError (Alonzo.AlonzoEra StandardCrypto)) Text where
    inject = Text.pack . show

instance Ledger.Inject (Ledger.AlonzoContextError (Babbage.BabbageEra StandardCrypto)) Text where
    inject = Text.pack . show

createCustomRedeemerAlonzo
  :: Ledger.PParams (Alonzo.AlonzoEra StandardCrypto)
  -> UTxO AlonzoEra
  -> EpochInfo (Either Text)
  -> SystemStart
  -> Api.Tx AlonzoEra
  -> Either (ScriptContextError AlonzoEra) (AnyCustomRedeemer AlonzoEra)
createCustomRedeemerAlonzo pparams utxo eInfo sStart (Api.ShelleyTx _ ledgerTx) = do
  let txBody = ledgerTx ^. Ledger.bodyTxL

  txcerts <- first DCertRelated
               . Prelude.mapM toPlutusCertsV1V2
               . seqToList $ txBody ^. Ledger.certsTxBodyL
  let witness = ledgerTx ^. Ledger.witsTxL
      sbe = alonzoEraOnwardsToShelleyBasedEra AlonzoEraOnwardsAlonzo
      outputs = seqToList $ txBody ^. Ledger.outputsTxBodyL
      ledgerUTxO = toLedgerUTxO sbe utxo
      minted = Ledger.transMintValue $ txBody ^. Ledger.mintTxBodyL
      Ledger.TxDats datumHashMap = witness ^. Ledger.datsTxWitsL
      datumHashes = Prelude.map Ledger.transDataPair $ Map.toList datumHashMap
      txsignatories = Prelude.map Ledger.transKeyHash . Set.toList $ txBody ^. Ledger.reqSignerHashesTxBodyL

  pTxIn <- transTxInInfoV1Alonzo txBody ledgerUTxO
  let txfee = Ledger.transCoinToValue $ txBody ^. Ledger.feeTxBodyL
      Ledger.AlonzoScriptsNeeded scriptsNeeded = Ledger.getScriptsNeeded ledgerUTxO
                                                   $ ledgerTx ^. Ledger.bodyTxL
  plutusScriptPurpose <- case scriptsNeeded of
                [(p ,_)] -> toScriptPurposeV1 p
                needed   -> Left $ MoreThanOneScriptPurpose needed
  let pTxOuts = mapMaybe Ledger.transTxOut outputs
  valRange <-
    first IntervalConvError
      $ Ledger.transValidityInterval pparams eInfo sStart
      $  txBody ^. Ledger.vldtTxBodyL
  Right . AnyPV1CustomRedeemer AlonzoEraOnwardsAlonzo
        $ PV1CustomRedeemer pTxOuts pTxIn minted valRange txfee
                            datumHashes txcerts txsignatories
                            (Just plutusScriptPurpose)


createCustomRedeemerBabbage
  :: Ledger.PParams (Babbage.BabbageEra StandardCrypto)
  -> UTxO BabbageEra
  -> EpochInfo (Either Text)
  -> SystemStart
  -> Api.Tx BabbageEra
  -> Either (ScriptContextError BabbageEra) (AnyCustomRedeemer BabbageEra)
createCustomRedeemerBabbage pparams utxo eInfo sStart (Api.ShelleyTx _ ledgerTx) = do
  let txBody = ledgerTx ^. Ledger.bodyTxL

  txcerts <- first DCertRelated
               . Prelude.mapM toPlutusCertsV1V2
               . seqToList $ txBody ^. Ledger.certsTxBodyL
  txin <- Prelude.maybe (Left MoreThanOneTxInput) Right
            $ listToMaybe $ Set.toList $ txBody ^. Ledger.inputsTxBodyL
  let witness = ledgerTx ^. Ledger.witsTxL
      sbe = alonzoEraOnwardsToShelleyBasedEra AlonzoEraOnwardsBabbage
      outputs = seqToList $ txBody ^. Ledger.outputsTxBodyL
      txOutputSource = Ledger.TxOutFromInput txin
      ledgerUTxO = toLedgerUTxO sbe utxo
      minted = Ledger.transMintValue $ txBody ^. Ledger.mintTxBodyL
      Ledger.TxDats datumHashMap = witness ^. Ledger.datsTxWitsL
      txsignatories = Prelude.map Ledger.transKeyHash . Set.toList $ txBody ^. Ledger.reqSignerHashesTxBodyL
  let Ledger.AlonzoScriptsNeeded scriptsNeeded = Ledger.getScriptsNeeded ledgerUTxO
                                                    $ ledgerTx ^. Ledger.bodyTxL
      referenceInputs = Set.toList $ txBody ^. Ledger.referenceInputsTxBodyL
  pTxIn <- first TxInRelated $ Ledger.transTxInInfoV2 ledgerUTxO txin
  pRefIns <- first TxInRelated
               $ Prelude.mapM (Ledger.transTxInInfoV2 ledgerUTxO) referenceInputs
  pTxOuts <- first TxOutRelated
                $ Prelude.mapM (Ledger.transTxOutV2 txOutputSource) outputs
  plutusScriptPurpose <- case scriptsNeeded of
                [(p ,_)] -> toScriptPurposeV2 p
                needed   -> Left $ MoreThanOneScriptPurpose needed
  txRedeemers <- first RedeemerRelated $ toTxRedeemersV2 ledgerTx
  valRange <-
    first IntervalConvError
      $ Ledger.transValidityInterval pparams eInfo sStart
      $  txBody ^. Ledger.vldtTxBodyL
  Right . AnyPV2CustomRedeemer BabbageEraOnwardsBabbage
   $ PV2CustomRedeemer
       { pv2Inputs = [pTxIn]
       , pv2RefInputs = pRefIns
       , pv2Outputs = pTxOuts
       , pv2Fee = PPrelude.mempty -- Possible to test but we would need to put the fee in a datum prior to the actual transaction context we want to test.
       , pv2Mint = minted
       , pv2DCert = txcerts
       , pv2Wdrl = PMap.empty -- TODO: Not tested
       , pv2ValidRange = valRange -- TODO: Fails when using (/=)
       , pv2Signatories = txsignatories
       , pv2Redeemers = txRedeemers
       , pv2Data = PMap.fromList . Prelude.map Ledger.transDataPair $ Map.toList datumHashMap
       , pv2ScriptPurpose = Just plutusScriptPurpose
       }

createAnyCustomRedeemer
  :: AlonzoEraOnwards era
  -> Ledger.PParams (Api.ShelleyLedgerEra era)
  -> UTxO era
  -> EpochInfo (Either Text)
  -> SystemStart
  -> Api.Tx era
  -> Either (ScriptContextError era) (AnyCustomRedeemer era)
createAnyCustomRedeemer AlonzoEraOnwardsAlonzo pparams utxo eInfo sStart tx = do
  createCustomRedeemerAlonzo pparams utxo eInfo sStart tx
createAnyCustomRedeemer AlonzoEraOnwardsBabbage pparams utxo eInfo sStart tx =
  createCustomRedeemerBabbage pparams utxo eInfo sStart tx
createAnyCustomRedeemer _ _ _ _ _ _ = Prelude.error "TODO: Conway"


transTxInInfoV1Alonzo
  :: Ledger.TxBody (Api.ShelleyLedgerEra AlonzoEra)
  -> Ledger.UTxO (Api.ShelleyLedgerEra AlonzoEra)
  -> Either (ScriptContextError era) [V1.TxInInfo]
transTxInInfoV1Alonzo txBody utxo = do
  insAndOuts <- forM (Set.toList (txBody ^. Ledger.inputsTxBodyL))
    $ \txIn -> do
         txOut <- first TxOutRelatedAlonzo $ transLookupTxOutAlonzo utxo txIn
         return (txIn,txOut)
  Right $ mapMaybe
    (\(txin,txout) -> PV1.TxInInfo (Ledger.transTxIn txin) Prelude.<$> Ledger.transTxOut txout)
    insAndOuts

transLookupTxOutAlonzo
  :: Ledger.UTxO (Api.ShelleyLedgerEra AlonzoEra)
  -> Ledger.TxIn (Legder.EraCrypto (Api.ShelleyLedgerEra AlonzoEra))
  -> Either Text (Ledger.TxOut (Api.ShelleyLedgerEra AlonzoEra))
transLookupTxOutAlonzo = Ledger.transLookupTxOut


seqToList :: Seq.StrictSeq a -> [a]
seqToList (x Seq.:<| rest) = x : seqToList rest
seqToList Seq.Empty        = []

toTxRedeemersV2
  :: Ledger.EraPlutusTxInfo Ledger.PlutusV2 (Api.ShelleyLedgerEra era)
  => Ledger.AlonzoEraTxBody (Api.ShelleyLedgerEra era)
  => Ledger.EraTx (Api.ShelleyLedgerEra era)
  => Ledger.AlonzoEraTxWits (Api.ShelleyLedgerEra era)
  => Ledger.Inject (Ledger.BabbageContextError (Api.ShelleyLedgerEra era))
                   (Ledger.ContextError (Api.ShelleyLedgerEra era))
  => Ledger.Tx (Api.ShelleyLedgerEra era)
  -> Either (Ledger.ContextError (Api.ShelleyLedgerEra era)) (PV2.Map V2.ScriptPurpose PV2.Redeemer)
toTxRedeemersV2 = Ledger.transTxRedeemers (Proxy :: Proxy Ledger.PlutusV2)

toScriptPurposeV1
  :: Ledger.EraPlutusTxInfo Ledger.PlutusV1 (Api.ShelleyLedgerEra era)
  => Ledger.PlutusPurpose Ledger.AsItem (Api.ShelleyLedgerEra era)
  -> Either (ScriptContextError era) V1.ScriptPurpose
toScriptPurposeV1 p =
  first ScriptPurposeRelated $ Ledger.toPlutusScriptPurpose (Proxy :: Proxy Ledger.PlutusV1) p

toScriptPurposeV2
  :: Ledger.EraPlutusTxInfo Ledger.PlutusV2 (Api.ShelleyLedgerEra era)
  => Ledger.PlutusPurpose Ledger.AsItem (Api.ShelleyLedgerEra era)
  -> Either (ScriptContextError era) V2.ScriptPurpose
toScriptPurposeV2 p =
  first ScriptPurposeRelated $ Ledger.toPlutusScriptPurpose (Proxy :: Proxy Ledger.PlutusV2) p

toPlutusCertsV1V2
 :: Ledger.EraPlutusTxInfo Ledger.PlutusV1 (Api.ShelleyLedgerEra era)
 => Ledger.TxCert (Api.ShelleyLedgerEra era)
 -> Either (Ledger.ContextError (Api.ShelleyLedgerEra era)) PV1.DCert
toPlutusCertsV1V2 = Ledger.toPlutusTxCert (Proxy :: Proxy Ledger.PlutusV1)

newtype CddlTx = CddlTx { unCddlTx :: InAnyShelleyBasedEra Api.Tx }
    deriving (Show, Eq)

cardanoNodePParams :: ConsensusModeParams
cardanoNodePParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

data AnyScriptContextEra where

  AnyScriptContextEra :: ScriptContextError era -> AnyScriptContextEra

createAnyCustomRedeemerFromTxFp
  :: SocketPath
  -> FilePath
  -> NetworkId
  -> ExceptT AnyScriptContextEra IO ACustomRedeemer
createAnyCustomRedeemerFromTxFp spath  fp network = do

  txFp <- liftIO $ fileOrPipe fp
  InAnyShelleyBasedEra sbe tx <- firstExceptT (AnyScriptContextEra . ReadTxBodyError)
                                           . newExceptT
                                           $ readFileTx txFp

  caseShelleyToMaryOrAlonzoEraOnwards
    (Prelude.const $ Prelude.error "ERRROR")
    (\aOnwards -> firstExceptT AnyScriptContextEra
                    $ ACustomRedeemer Prelude.<$>  shelleyBasedEraConstraints sbe
                        (createAnyCustomRedeemerFromTx aOnwards spath tx network)
    )
    sbe


createAnyCustomRedeemerFromTx
  :: AlonzoEraOnwards era
  -> SocketPath
  -> Api.Tx era
  -> NetworkId
  -> ExceptT (ScriptContextError era) IO (AnyCustomRedeemer era)
createAnyCustomRedeemerFromTx aOnwards spath tx network = do
  let localNodeConnInfo = LocalNodeConnectInfo cardanoNodePParams network spath
  res <- firstExceptT QueryAcquiringFailure $ newExceptT
           $ executeLocalStateQueryExpr localNodeConnInfo VolatileTip
           $ runExceptT $ do
               let sbe = alonzoEraOnwardsToShelleyBasedEra aOnwards
               (EraHistory interpreter) <- lift queryEraHistory & onLeft (left . QueryFailure)
               sStart <- lift querySystemStart & onLeft (left . QueryFailure)
               let eInfo = hoistEpochInfo (first (T.pack . docToString . prettyError . TransactionValidityIntervalError) . runExcept)
                             $ Consensus.interpreterToEpochInfo interpreter

               pparams <- lift (queryProtocolParameters sbe)
                                & onLeft (left . QueryFailure)
                                & onLeft (left . QueryFailureEra)
               utxo <- lift (queryUtxo sbe QueryUTxOWhole)
                         & onLeft (left . QueryFailure)
                         & onLeft (left . QueryFailureEra)

               hoistEither $ createAnyCustomRedeemer
                                     aOnwards
                                     pparams
                                     utxo
                                     eInfo
                                     sStart
                                     tx

  hoistEither res



createAnyCustomRedeemerBsFromTxFp
  :: SocketPath
  -> FilePath
  -> NetworkId
  -> ExceptT AnyScriptContextEra IO LB.ByteString
createAnyCustomRedeemerBsFromTxFp sPath txFp  nid = do
  ACustomRedeemer anyCustomRedeemer <- createAnyCustomRedeemerFromTxFp sPath txFp  nid
  return . Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
         $ Api.fromAlonzoData
         $ customRedeemerToScriptData anyCustomRedeemer


-- Used in roundtrip testing

fromPlutusTxId :: V1.TxId -> Ledger.TxId StandardCrypto
fromPlutusTxId (V1.TxId builtInBs) =
  case deserialiseFromRawBytes AsTxId $ fromBuiltin builtInBs of
    Right txidHash -> Api.toShelleyTxId txidHash
    Left e         -> Prelude.error $ "Could not derserialize txid: " <> show e


sampleTestV1ScriptContextDataJSON :: LB.ByteString
sampleTestV1ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . Api.fromAlonzoData
    . customRedeemerToScriptData
    . AnyPV1CustomRedeemer AlonzoEraOnwardsAlonzo
    $ PV1CustomRedeemer
        dummyTxOuts
        dummyTxIns
        dummyLedgerVal
        dummyPOSIXTimeRange
        dummyLedgerVal
        dummyDatumHashes
        dummyCerts
        dummySignatories
        dummyScriptPurpose


dummyCerts :: [V1.DCert]
dummyCerts = []

dummyTxIns :: [V1.TxInInfo]
dummyTxIns = []

dummySignatories :: [V1.PubKeyHash]
dummySignatories = []

dummyDatumHashes :: [(V1.DatumHash, V1.Datum)]
dummyDatumHashes = []

dummyLedgerVal :: V1.Value
dummyLedgerVal = undefined -- Alonzo.transValue $ toMaryValue Prelude.mempty

dummyTxOuts :: [V1.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: V1.POSIXTimeRange
dummyPOSIXTimeRange = V1.from $ V1.POSIXTime 42

dummyScriptPurpose :: Maybe V1.ScriptPurpose
dummyScriptPurpose = Nothing

sampleTestV2ScriptContextDataJSON :: LB.ByteString
sampleTestV2ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . Api.fromAlonzoData
    . customRedeemerToScriptData
    . AnyPV2CustomRedeemer BabbageEraOnwardsBabbage
    $ PV2CustomRedeemer
       { pv2Inputs = []
       , pv2RefInputs = []
       , pv2Outputs = []
       , pv2Fee = PPrelude.mempty
       , pv2Mint = PPrelude.mempty
       , pv2DCert = []
       , pv2Wdrl = PMap.empty
       , pv2ValidRange = V2.always
       , pv2Signatories = []
       , pv2Redeemers = PMap.empty
       , pv2Data = PMap.empty
       , pv2ScriptPurpose = Nothing
       }
