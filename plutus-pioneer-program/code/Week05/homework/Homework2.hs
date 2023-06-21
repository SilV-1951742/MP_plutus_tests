{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext, TokenName, TxOutRef,
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.), any, Eq ((==)), (&&), emptyByteString)
import           Utilities            (wrapPolicy)
import Plutus.V2.Ledger.Contexts
import PlutusTx.Trace
import Plutus.V1.Ledger.Value

{-# INLINABLE tn #-}
tn :: TokenName
tn = TokenName emptyByteString

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy _oref () _ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                 traceIfFalse "wrong amount minted" checkMintedAmount 
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == _oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol _ctx && tn' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
