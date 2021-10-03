{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Market.Offchain
    ( endpoints
    )
    where

import qualified Data.Map                  as Map
import           Data.Monoid               as Mnd ( (<>) )
import           Control.Monad             ( void, forever )
import           Data.Aeson                (ToJSON)
import           Data.Text                 (Text)
import           Prelude                   (String)


import Plutus.Contract as Contract
    ( AsContractError,
      logError,
      logInfo,
      awaitTxConfirmed,
      endpoint,
      ownPubKey,
      submitTxConstraintsWith,
      utxosAt,
      handleError,
      select,
      Contract,
      Promise(awaitPromise) )
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( return, Bool, Maybe(..), Eq((==)), (<$>), ($) )
import Ledger
    ( scriptAddress,
      pubKeyHash,
      txId,
      pubKeyHashAddress,
      Redeemer(Redeemer),
      TxOut(txOutValue),
      TxOutRef,
      ChainIndexTxOut, toTxOut )
import Ledger.Constraints as Constraints
    ( otherScript,
      typedValidatorLookups,
      unspentOutputs,
      mustPayToPubKey,
      mustPayToTheScript,
      mustSpendScriptOutput )
import Ledger.Value as Value
    ( singleton,
      AssetClass(AssetClass),
      assetClassValueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)

import           Market.Types               (NFTSale(..), SaleAction(..), SaleSchema)
import           Market.Onchain             ( Sale, typedBuyValidator, buyValidator )


startSale :: NFTSale -> Contract w SaleSchema Text ()
startSale nfts = do 
    utxos <- utxosAt (pubKeyHashAddress $ nSeller nfts)
    let val     = Value.singleton (nCurrency nfts) (nToken nfts) 1
        lookups = Constraints.unspentOutputs utxos                         <>
                  Constraints.typedValidatorLookups (typedBuyValidator nfts)
        tx      = Constraints.mustPayToTheScript () val
    ledgerTx <- submitTxConstraintsWith @Sale lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String "startSale transaction confirmed"


buy :: NFTSale -> Contract w SaleSchema Text ()
buy nfts = do   
    pkh <- pubKeyHash <$> Contract.ownPubKey
    sale <- findSale nfts
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Buy
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1
                valAda  = Ada.lovelaceValueOf $ nPrice nfts
                lookups = Constraints.typedValidatorLookups (typedBuyValidator nfts) <>
                          Constraints.unspentOutputs (Map.singleton oref o)          <>
                          Constraints.otherScript (buyValidator nfts)
                tx      = Constraints.mustSpendScriptOutput oref r          <>
                          Constraints.mustPayToPubKey pkh val               <>
                          Constraints.mustPayToPubKey (nSeller nfts) valAda
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String "buy transaction confirmed"


close :: NFTSale -> Contract w SaleSchema Text ()
close nfts = do
    sale <- findSale nfts
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1
                lookups = Constraints.typedValidatorLookups (typedBuyValidator nfts) <>
                          Constraints.otherScript (buyValidator nfts)                <>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r      <>
                          Constraints.mustPayToPubKey (nSeller nfts) val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String "close transaction confirmed"


findSale :: (AsContractError e, ToJSON e) => NFTSale -> Contract w SaleSchema e (Maybe (TxOutRef, ChainIndexTxOut))
findSale nfts = do
    utxos <- Map.filter f <$> utxosAt (scriptAddress $ buyValidator nfts)
    return $ case Map.toList utxos of
        [(oref, o)] -> return (oref, o)
        _           -> Nothing

  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (txOutValue $ toTxOut o) (AssetClass (nCurrency nfts, nToken nfts)) == 1


endpoints :: Contract () SaleSchema Text ()
endpoints = forever
          $ handleError logError
          $ awaitPromise
          $ start' `select` buy' `select` close'
  where
    start' = endpoint @"start" $ \nfts -> startSale nfts
    buy'   = endpoint @"buy"   $ \nfts -> buy nfts
    close' = endpoint @"close" $ \nfts -> close nfts