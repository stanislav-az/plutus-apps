{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Stablecoin(
    tests
    , stablecoinTrace
    , maxReservesExceededTrace
    ) where


import Control.Lens (preview, (&))
import Control.Monad (void)
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (Rational, negate)

import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (PaymentPubKey))
import Ledger.CardanoWallet qualified as CW
import Ledger.Crypto (toPublicKey)
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts (validatorAddress)
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.Contract.Oracle (Observation, SignedMessage, signObservation')
import Plutus.Contract.Test
import Plutus.Contracts.Stablecoin (BC (..), ConversionRate, Input (..), RC (..), SC (..), SCAction (..),
                                    Stablecoin (..), StablecoinError, StablecoinSchema)
import Plutus.Contracts.Stablecoin qualified as Stablecoin
import Plutus.Trace.Emulator (ContractHandle, EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types (_ContractLog, cilMessage)
import PlutusTx.Numeric (negate, one, zero)
import PlutusTx.Ratio qualified as R
import Wallet.Emulator.MultiAgent (eteEvent)

import Test.Tasty

user :: Wallet
user = w1

oraclePrivateKey :: PaymentPrivateKey
oraclePrivateKey = CW.paymentPrivateKey $ CW.fromWalletNumber $ CW.WalletNumber 2

onePercent :: R.Rational
onePercent = R.unsafeRatio 1 100

coin :: Stablecoin
coin = Stablecoin
    { scOracle = PaymentPubKey $ toPublicKey (unPaymentPrivateKey oraclePrivateKey)
    , scFee = onePercent
    , scMinReserveRatio = zero
    , scMaxReserveRatio = R.unsafeRatio 4 1
    , scReservecoinDefaultPrice = BC 1
    , scBaseCurrency = Value.assetClass adaSymbol adaToken
    , scStablecoinTokenName = "stablecoin"
    , scReservecoinTokenName = "reservecoin"
    }

signConversionRate :: POSIXTime -> ConversionRate -> SignedMessage (Observation ConversionRate)
signConversionRate startTime rate = signObservation' startTime rate (unPaymentPrivateKey oraclePrivateKey)

stablecoinAddress :: Address
stablecoinAddress = validatorAddress $ Stablecoin.typedValidator coin

initialDeposit :: Value
initialDeposit = Ada.lovelaceValueOf 10_000_000

initialFee :: Value
initialFee = Ada.lovelaceValueOf 100_000 -- Defined as 1% of initialDeposit

tests :: TestTree
tests = testGroup "Stablecoin"
    [ checkPredicateOptions (defaultCheckOptions & allowBigTransactions) "mint reservecoins"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.reserveCoins coin 10_000_000 <> negate (initialDeposit <> initialFee))
        )
        $ initialise >>= mintReserveCoins (RC 10_000_000) one

    , checkPredicateOptions (defaultCheckOptions & allowBigTransactions) "mint reservecoins and stablecoins"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Ada.lovelaceValueOf 5_050_000))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.stableCoins coin 5_000_000 <> Stablecoin.reserveCoins coin 10_000_000 <> negate (initialDeposit <> initialFee <> Ada.lovelaceValueOf 5_050_000))
        )
        $ do
            hdl <- initialise
            mintReserveCoins (RC 10_000_000) one hdl
            -- Mint 50 stablecoins at a rate of 1 Ada: 1 USD
            void $ mintStableCoins (SC 5_000_000) one hdl

    , checkPredicateOptions (defaultCheckOptions & allowBigTransactions) "mint reservecoins, stablecoins and redeem stablecoin at a different price"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Ada.lovelaceValueOf 1_090_000))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.stableCoins coin 3_000_000 <> Stablecoin.reserveCoins coin 10_000_000 <> negate (initialDeposit <> initialFee <> Ada.lovelaceValueOf 1_090_000))
        )
        stablecoinTrace

    , let expectedLogMsg = "New state is invalid: MaxReserves {allowed = BC {unBC = Rational 20000000 1}, actual = BC {unBC = Rational 20173235 1}}. The transition is not allowed." in
      checkPredicateOptions (defaultCheckOptions & allowBigTransactions) "Cannot exceed the maximum reserve ratio"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Ada.lovelaceValueOf 5_050_000))
        .&&. assertNoFailedTransactions
        .&&. assertInstanceLog (Trace.walletInstanceTag w1) ((==) (Just expectedLogMsg) . listToMaybe . reverse . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
        )
        maxReservesExceededTrace

    ]

initialise :: Trace.EmulatorTrace (ContractHandle () StablecoinSchema StablecoinError)
initialise = do
    hdl <- Trace.activateContractWallet user Stablecoin.contract
    Trace.callEndpoint @"initialise" hdl coin
    _ <- Trace.waitNSlots 2
    pure hdl

mintReserveCoins :: RC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
mintReserveCoins rc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintReserveCoin rc
            }
    void $ Trace.waitNSlots 2

mintStableCoins :: SC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
mintStableCoins sc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintStablecoin sc
            }
    void $ Trace.waitNSlots 2

redeemStableCoins :: SC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
redeemStableCoins sc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintStablecoin (negate sc)
            }
    void $ Trace.waitNSlots 2

-- | Mint 100 reserve coins, mint 50 stablecoins, then redeem ten of
--   them at a higher exchange rate
stablecoinTrace :: EmulatorTrace ()
stablecoinTrace = do
    hdl <- initialise
    mintReserveCoins (RC 10_000_000) one hdl
    mintStableCoins (SC 5_000_000) one hdl
    -- redeem 2M stablecoins at an exchange rate of 2 Ada : 1 USD (so we get 4 Ada from the bank)
    redeemStableCoins (SC 2_000_000) (R.fromInteger 2) hdl

-- | Mint 100 reserve coins, mint 50 stablecoins, then attempt to mint
--   another 49 reserve coins. This fails because the max. reserve ratio
--   would be exceeded.
maxReservesExceededTrace :: EmulatorTrace ()
maxReservesExceededTrace = do
    hdl <- initialise
    mintReserveCoins (RC 10_000_000) one hdl
    mintStableCoins (SC 5_000_000) one hdl

    -- At this point we have:
    -- Stablecoins: 50 (equiv. to 50 Lovelace on the 1:1 conversion
    -- rate)
    -- Max. reserve ratio: 4:1
    -- Reserves: 151 Lovelace (100 from minting reserve coins, 50 from
    -- minting stablecoins, 1 from fees)
    -- Maximum reserves: 200 Lovelace (50 stablecoins * 4 (Lovelace /
    -- stablecoin))

    -- The next transition is not allowed as it would bring the reserve
    -- ratio above the maximum.
    mintReserveCoins (RC 4_900_000) one hdl
