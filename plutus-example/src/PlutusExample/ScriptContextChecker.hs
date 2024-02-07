{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}



module PlutusExample.ScriptContextChecker where

import           Prelude                                                 hiding
                                                                         (($))

import           Cardano.Api
import           Cardano.Api.Ledger
import           Cardano.Api.Pretty
import qualified Cardano.Api.Shelley                                     as Api

import qualified Cardano.Ledger.Plutus.Language                          as Ledger
import           Control.Monad
import           Control.Monad.IO.Class                                  (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson                                              as Aeson
import           Data.Bifunctor
import           Data.Bifunctor                                          (first)
import qualified Data.ByteString.Lazy                                    as LB
import qualified Data.Map.Strict                                         as Map
import           Data.Proxy
import qualified Data.Sequence.Strict                                    as Seq
import qualified Data.Set                                                as Set
import qualified Data.Text                                               as T
import           Data.Word
import           GHC.Records                                             (HasField (..))

import qualified Cardano.Ledger.Alonzo                                   as Alonzo
import qualified Cardano.Ledger.Alonzo.Plutus.Context                    as Ledger
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo                     as Ledger
import qualified Cardano.Ledger.Alonzo.PParams                           as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts                           as Ledger
import qualified Cardano.Ledger.Alonzo.Tx                                as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody                            as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits                            as Ledger
import qualified Cardano.Ledger.Alonzo.UTxO                              as Ledger
import           Cardano.Ledger.Babbage.PParams
import qualified Cardano.Ledger.Babbage.TxBody                           as Ledger
import qualified Cardano.Ledger.Babbage.TxInfo                           as Babbage
import qualified Cardano.Ledger.Babbage.TxInfo                           as Ledger
import qualified Cardano.Ledger.Shelley.TxCert                           as Ledger
import qualified PlutusLedgerApi.V2                                      as PV2

import qualified Cardano.Ledger.BaseTypes                                as Ledger
import           Cardano.Ledger.Binary.Decoding
import qualified Cardano.Ledger.Coin                                     as Ledger
import qualified Cardano.Ledger.Conway.TxInfo                            as Ledger
import qualified Cardano.Ledger.Core                                     as Ledger
import           Cardano.Ledger.Crypto                                   (StandardCrypto)
import qualified Cardano.Ledger.Mary.TxBody                              as Ledger
import qualified Cardano.Ledger.Plutus.TxInfo                            as Ledger
import qualified Cardano.Ledger.Shelley.API                              as Shelley
import           Cardano.Ledger.Shelley.Tx                               ()
import qualified Cardano.Ledger.TxIn                                     as Ledger
import qualified Cardano.Ledger.UTxO                                     as Ledger
import           Cardano.Slotting.EpochInfo                              (EpochInfo,
                                                                          hoistEpochInfo)
import           Control.Monad.Trans.Except
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras      (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras      as Consensus
import qualified Ouroboros.Consensus.HardFork.History                    as Consensus
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import qualified PlutusLedgerApi.V1                                      as PV1

import           PlutusLedgerApi.V1                                      as V1
import           PlutusLedgerApi.V2                                      as V2
import qualified PlutusTx.AssocMap                                       as PMap
import           PlutusTx.Prelude                                        as PPrelude hiding
                                                                                     (Eq,
                                                                                      Semigroup (..),
                                                                                      unless,
                                                                                      (.))

import           Cardano.CLI.Read
import           Data.Maybe                                              (catMaybes)
import           Data.Text                                               (Text)
import           Lens.Micro
import           PlutusExample.PlutusVersion1.RedeemerContextScripts
import           PlutusExample.PlutusVersion2.RedeemerContextEquivalence

data AnyCustomRedeemer
  = AnyPV1CustomRedeemer PV1CustomRedeemer
  | AnyPV2CustomRedeemer PV2CustomRedeemer
  deriving (Show, Eq)

-- We convert our custom redeemer to ScriptData so we can include it
-- in our transaction.
customRedeemerToScriptData :: AnyCustomRedeemer -> ScriptData
customRedeemerToScriptData (AnyPV1CustomRedeemer cRedeem) =
  Api.fromPlutusData $ V1.toData cRedeem
customRedeemerToScriptData (AnyPV2CustomRedeemer cRedeem) =
  Api.fromPlutusData $ V2.toData cRedeem


data ScriptContextError era
  = ReadTxBodyError CddlError
  | DCertRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | TxInRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
  | TxOutRelated (Ledger.ContextError (Api.ShelleyLedgerEra era))
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
                 -- deriving Show


--createRedeemerConstraints
--  :: AlonzoEraOnwards era
--  -> ( => a)
--  -> a


customRedeemerConstraints
  :: AlonzoEraOnwards era
  -> (( Ledger.EraTxBody (Api.ShelleyLedgerEra era)
     ) => a)
  -> a
customRedeemerConstraints AlonzoEraOnwardsAlonzo f  = f
customRedeemerConstraints AlonzoEraOnwardsBabbage f = f
customRedeemerConstraints AlonzoEraOnwardsConway f  = f



createAnyCustomRedeemer
  :: Ledger.EraTxBody (Api.ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Ledger.PParams (Api.ShelleyLedgerEra era)
  -> UTxO era
  -> EpochInfo (Either Text)
  -> SystemStart
  -> Api.Tx era
  -> Either (ScriptContextError era) AnyCustomRedeemer
createAnyCustomRedeemer sbe pparams utxo eInfo sStart (Api.ShelleyTx _ ledgerTx) = do
  let txBody = ledgerTx ^. Ledger.bodyTxL
  txcerts <- first DCertRelated
               . Prelude.mapM toPlutusCertsV1V2
               . seqToList $ txBody ^. Ledger.certsTxBodyL
  txin <- Prelude.maybe (Left MoreThanOneTxInput) Right
            $ listToMaybe $ Set.toList $ txBody ^. Ledger.inputsTxBodyL
  let witness = ledgerTx ^. Ledger.witsTxL
      outputs = seqToList $ txBody ^. Ledger.outputsTxBodyL
      txOutputSource = Ledger.TxOutFromInput txin
      referenceInputs = Set.toList $ txBody ^. Ledger.referenceInputsTxBodyL
      ledgerUTxO = toLedgerUTxO sbe utxo
      minted = Ledger.transMintValue $ txBody ^. Ledger.mintTxBodyL
      Ledger.TxDats datumHashMap = witness ^. Ledger.datsTxWitsL
      datumHashes = Prelude.map Ledger.transDataPair $ Map.toList datumHashMap
      txsignatories = Prelude.map Ledger.transKeyHash . Set.toList $ txBody ^. Ledger.reqSignerHashesTxBodyL
  valRange <-
    first IntervalConvError
      $ Ledger.transValidityInterval pparams eInfo sStart $  txBody ^. Ledger.vldtTxBodyL

  case sbe of
    ShelleyBasedEraShelley -> Prelude.error "TODO: Improve error"
    ShelleyBasedEraAllegra -> Prelude.error "TODO: Improve error"
    ShelleyBasedEraMary -> Prelude.error "TODO: Improve error"
    ShelleyBasedEraAlonzo -> do
        let txfee = Ledger.inject $ txBody ^. Ledger.feeTxBodyL
            Ledger.AlonzoScriptsNeeded scriptsNeeded = Ledger.getScriptsNeeded ledgerUTxO
                                                         $ ledgerTx ^. Ledger.bodyTxL
        plutusScriptPurpose <- case scriptsNeeded of
                      [(p ,_)] -> toScriptPurposeV1 p
                      needed   -> Left $ MoreThanOneScriptPurpose needed
        pTxIn <- first TxInRelated $ Ledger.transTxInInfoV1 ledgerUTxO txin
        pTxOuts <- first TxOutRelated
                     $ Prelude.mapM (Ledger.transTxOutV1 txOutputSource) outputs


        Right . AnyPV1CustomRedeemer
              $ PV1CustomRedeemer pTxOuts [pTxIn] minted valRange txfee
                                  datumHashes txcerts txsignatories
                                  (Just plutusScriptPurpose)

    ShelleyBasedEraBabbage -> do
       let Ledger.AlonzoScriptsNeeded scriptsNeeded = Ledger.getScriptsNeeded ledgerUTxO
                                                         $ ledgerTx ^. Ledger.bodyTxL
       pTxIn <- first TxInRelated $ Ledger.transTxInInfoV2 ledgerUTxO txin
       pRefIns <- first TxInRelated
                    $ Prelude.mapM (Ledger.transTxInInfoV2 ledgerUTxO) referenceInputs
       pTxOuts <- first TxOutRelated
                     $ Prelude.mapM (Ledger.transTxOutV2 txOutputSource) outputs
       plutusScriptPurpose <- case scriptsNeeded of
                     [(p ,_)] -> toScriptPurposeV2 p
                     needed   -> Left $ MoreThanOneScriptPurpose needed
       txRedeemers <- first RedeemerRelated $ toTxRedeemersV2 ledgerTx

       Right . AnyPV2CustomRedeemer
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
    ShelleyBasedEraConway -> Prelude.error "TODO"


seqToList :: Seq.StrictSeq a -> [a]
seqToList (x Seq.:<| rest) = x : seqToList rest
seqToList Seq.Empty        = []

toTxRedeemersV2
  :: Ledger.EraTx (Api.ShelleyLedgerEra era)
  => Ledger.Tx (Api.ShelleyLedgerEra era)
  -> Either (Ledger.ContextError (Api.ShelleyLedgerEra era)) (PV2.Map V2.ScriptPurpose PV2.Redeemer)
toTxRedeemersV2 = Ledger.transTxRedeemers (Proxy :: Proxy Ledger.PlutusV2)

toScriptPurposeV1
  :: Ledger.EraPlutusContext (Api.ShelleyLedgerEra era)
  => Ledger.PlutusPurpose Ledger.AsItem (Api.ShelleyLedgerEra era)
  -> Either (ScriptContextError era) V1.ScriptPurpose
toScriptPurposeV1 p =
  first ScriptPurposeRelated $ Ledger.toPlutusScriptPurpose (Proxy :: Proxy Ledger.PlutusV1) p

toScriptPurposeV2
  :: Ledger.EraPlutusContext (Api.ShelleyLedgerEra era)
  => Ledger.PlutusPurpose Ledger.AsItem (Api.ShelleyLedgerEra era)
  -> Either (ScriptContextError era) V2.ScriptPurpose
toScriptPurposeV2 p =
  first ScriptPurposeRelated $ Ledger.toPlutusScriptPurpose (Proxy :: Proxy Ledger.PlutusV2) p

toPlutusCertsV1V2
 :: Ledger.ShelleyEraTxCert (Api.ShelleyLedgerEra era)
 => Ledger.TxCert (Api.ShelleyLedgerEra era)
 -> Either (Ledger.ContextError (Api.ShelleyLedgerEra era)) PV1.DCert
toPlutusCertsV1V2 = Ledger.toPlutusTxCert (Proxy :: Proxy Ledger.PlutusV1)
newtype CddlTx = CddlTx { unCddlTx :: InAnyShelleyBasedEra Api.Tx }
    deriving (Show, Eq)

cardanoNodePParams :: ConsensusModeParams
cardanoNodePParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600


createAnyCustomRedeemerFromTxFp
  :: ShelleyBasedEra era
  -> SocketPath
  -> Api.Tx era
  -> FilePath
  -> NetworkId
  -> ExceptT (ScriptContextError era) IO AnyCustomRedeemer
createAnyCustomRedeemerFromTxFp sbe spath tx fp network = do
 -- txFp <- liftIO $ fileOrPipe fp
 -- InAnyShelleyBasedEra sbe tx <- firstExceptT ReadTxBodyError
 --                                          . newExceptT
 --                                          $ readFileTx txFp

---
  let localNodeConnInfo = LocalNodeConnectInfo cardanoNodePParams network spath
  res <- firstExceptT QueryAcquiringFailure $ newExceptT $ executeLocalStateQueryExpr localNodeConnInfo VolatileTip
           $ runExceptT $ do
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
                                     sbe
                                     pparams
                                     utxo
                                     eInfo
                                     sStart
                                     tx

  hoistEither res









createAnyCustomRedeemerBsFromTxFp
  :: PlutusScriptVersion lang
  -> FilePath
  -> NetworkId
  -> ExceptT (ScriptContextError era) IO LB.ByteString
createAnyCustomRedeemerBsFromTxFp pScriptVer txFp  nid = do
  anyCustomRedeemer <- createAnyCustomRedeemerFromTxFp pScriptVer txFp anyCmodeParams nid
  return . Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
         $ customRedeemerToScriptData anyCustomRedeemer


-- Used in roundtrip testing

fromPlutusTxId :: V1.TxId -> Ledger.TxId StandardCrypto
fromPlutusTxId (V1.TxId builtInBs) =
  case deserialiseFromRawBytes AsTxId $ fromBuiltin builtInBs of
    Just txidHash -> toShelleyTxId txidHash
    Nothing       -> Prelude.error "Could not derserialize txid"


sampleTestV1ScriptContextDataJSON :: LB.ByteString
sampleTestV1ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    . AnyPV1CustomRedeemer
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
dummyLedgerVal = Alonzo.transValue $ toMaryValue Prelude.mempty

dummyTxOuts :: [V1.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: V1.POSIXTimeRange
dummyPOSIXTimeRange = V1.from $ V1.POSIXTime 42

dummyScriptPurpose :: Maybe V1.ScriptPurpose
dummyScriptPurpose = Nothing

-- TODO: Left off here
getTxInInfoFromTxIn
  :: Ledger.EraPlutusTxInfo l (Api.ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Proxy l
  -> Ledger.PParams (Api.ShelleyLedgerEra era)
  -> Shelley.UTxO (Api.ShelleyLedgerEra era)
  -> Ledger.TxIn (EraCrypto (Api.ShelleyLedgerEra era))
  -> Either (Ledger.ContextError (Api.ShelleyLedgerEra era)) (Ledger.PlutusTxInfo l)
getTxInInfoFromTxIn sbe p pparams (Shelley.UTxO utxoMap) txIn = do
  case Map.lookup txIn utxoMap of
    Nothing -> undefined
    Just txOut -> Ledger.toPlutusTxInfo p pparams undefined undefined undefined undefined

sampleTestV2ScriptContextDataJSON :: LB.ByteString
sampleTestV2ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    . AnyPV2CustomRedeemer
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
