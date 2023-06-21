{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       Validator, TxInfo (txInfoValidRange),
                                       ScriptContext (scriptContextTxInfo),
                                       mkValidatorScript, POSIXTimeRange)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, (&&), (||), (+))
import           Utilities            (wrapValidator)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = traceIfFalse "beneficiary1's signature missing or too late" (signedByBeneficiary1 && deadlineNotReached) ||
                                  traceIfFalse "beneficiary2's signature missing or too early" (signedByBeneficiary2 && deadlineReached)
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    txValidRange :: POSIXTimeRange
    txValidRange  = txInfoValidRange info

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 = txSignedBy info (beneficiary1 _dat)

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info (beneficiary2 _dat)

    deadlineNotReached :: Bool
    deadlineNotReached = contains (to (deadline _dat)) txValidRange

    deadlineReached :: Bool
    deadlineReached = contains (from (1 + deadline _dat)) txValidRange

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
