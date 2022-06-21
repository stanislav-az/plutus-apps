{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion2.RedeemerContextEquivalence
  ( v2ScriptContextEquivalenceScript
  , v2ScriptContextEquivalenceSbs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&))

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V2.Scripts.Validators as V2
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Prelude as PlutusPrelude hiding (Semigroup (..), unless, (.))

newtype MyCustomDatumV2 = MyCustomDatumV2 Integer

data PV2CustomRedeemer
  = PV2CustomRedeemer
      { pv2Inputs      :: [V2.TxInInfo]
      , pv2RefInputs   :: [V2.TxInInfo]
      , pv2Outputs     :: [V2.TxOut]
      , pv2Fee         :: V2.Value
      , pv2Mint        :: V2.Value
      , pv2DCert       :: [V2.DCert]
      , pv2Wdrl        :: V2.Map V2.StakingCredential Integer
      , pv2ValidRange  :: V2.POSIXTimeRange
      , pv2Signatories :: [V2.PubKeyHash]
      , pv2Redeemers   :: V2.Map ScriptPurpose V2.Redeemer
      , pv2Data        :: V2.Map V2.DatumHash V2.Datum
      }

PlutusTx.unstableMakeIsData ''MyCustomDatumV2
PlutusTx.unstableMakeIsData ''PV2CustomRedeemer

-- @(PV2CustomRedeemer inputs refInputs outputs fee mint dCert wdrl validRange signatories redeemers data)

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatumV2 -> PV2CustomRedeemer -> V2.ScriptContext -> Bool
mkValidator _ redeemer scriptContext =
  inputsAreEquivalent redeemer txInfo
 where
  txInfo :: V2.TxInfo
  txInfo = V2.scriptContextTxInfo scriptContext

  inputsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  inputsAreEquivalent (PV2CustomRedeemer inputs _ _ _ _ _ _ _ _ _ _) tInfo =
    (PlutusPrelude.map txInInfoResolved $ V2.txInfoInputs tInfo) PlutusPrelude.==
    (PlutusPrelude.map txInInfoResolved inputs)

validator :: V2.Validator
validator = V2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2.mkUntypedValidator mkValidator

v2ScriptContextEquivalencePlutusScript :: V2.Script
v2ScriptContextEquivalencePlutusScript = V2.unValidatorScript validator

v2ScriptContextEquivalenceSbs :: SBS.ShortByteString
v2ScriptContextEquivalenceSbs =
  SBS.toShort . LBS.toStrict $ serialise v2ScriptContextEquivalencePlutusScript

v2ScriptContextEquivalenceScript :: PlutusScript PlutusScriptV2
v2ScriptContextEquivalenceScript = PlutusScriptSerialised v2ScriptContextEquivalenceSbs
{-
      txInfoInputs          :: [TxInInfo] -- ^ Transaction inputs
    , txInfoReferenceInputs :: [TxInInfo] -- ^ Transaction reference inputs
    , txInfoOutputs         :: [TxOut] -- ^ Transaction outputs
    , txInfoFee             :: Value -- ^ The fee paid by this transaction.
    , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert           :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl            :: Map StakingCredential Integer -- ^ Withdrawals
    , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories     :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: Map ScriptPurpose Redeemer
    , txInfoData            :: Map DatumHash Datum
    , txInfoId              :: TxId

-}
