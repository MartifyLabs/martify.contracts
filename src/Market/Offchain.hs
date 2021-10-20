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
      utxosTxOutTxAt,
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
      CurrencySymbol,
      TokenName,
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
      valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)
import Plutus.ChainIndex.Tx ( ChainIndexTx(_citxData) )

import           Market.Types               (NFTSale(..), SaleAction(..), SaleSchema, StartParams(..), BuyParams(..))
import           Market.Onchain             ( Sale, typedBuyValidator, buyValidator, nftDatum )



startSale :: StartParams -> Contract w SaleSchema Text ()
startSale sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    utxos <- utxosAt (pubKeyHashAddress pkh)
    let val     = Value.singleton (sCs sp) (sTn sp) 1
        dat     = NFTSale { nSeller = pkh, nToken = sTn sp, nCurrency = sCs sp, nPrice = sPrice sp }
        lookups = Constraints.unspentOutputs utxos                         <>
                  Constraints.typedValidatorLookups typedBuyValidator
        tx      = Constraints.mustPayToTheScript dat val
    ledgerTx <- submitTxConstraintsWith @Sale lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String "startSale transaction confirmed"


buy :: BuyParams -> Contract w SaleSchema Text ()
buy bp = do   
    pkh <- pubKeyHash <$> Contract.ownPubKey
    sale <- findSale (bCs bp, bTn bp)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, nfts) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Buy
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1 <> Ada.lovelaceValueOf 2000000
                valAda  = Ada.lovelaceValueOf $ nPrice nfts
                lookups = Constraints.typedValidatorLookups typedBuyValidator <>
                          Constraints.unspentOutputs (Map.singleton oref o)          <>
                          Constraints.otherScript buyValidator
                tx      = Constraints.mustSpendScriptOutput oref r          <>
                          Constraints.mustPayToPubKey pkh val               <>
                          Constraints.mustPayToPubKey (nSeller nfts) valAda
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String "buy transaction confirmed"


close :: BuyParams -> Contract w SaleSchema Text ()
close bp = do
    sale <- findSale (bCs bp, bTn bp)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, nfts) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1
                lookups = Constraints.typedValidatorLookups typedBuyValidator <>
                          Constraints.otherScript buyValidator                <>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r      <>
                          Constraints.mustPayToPubKey (nSeller nfts) val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String "close transaction confirmed"


findSale :: (AsContractError e, ToJSON e) => (CurrencySymbol, TokenName) -> Contract w SaleSchema e (Maybe (TxOutRef, ChainIndexTxOut, NFTSale))
findSale (cs, tn) = do
    utxos <- Map.filter f <$> utxosTxOutTxAt (scriptAddress buyValidator)
    return $ case Map.toList utxos of
        [(oref, (o, citx))] -> do
            nftSale <- nftDatum (toTxOut o) $ \dh -> Map.lookup dh $ _citxData citx
            Just (oref, o, nftSale)
        _           -> Nothing

  where
    f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
    f (o, _) = valueOf (txOutValue $ toTxOut o) cs tn == 1


endpoints :: Contract () SaleSchema Text ()
endpoints = forever
          $ handleError logError
          $ awaitPromise
          $ start' `select` buy' `select` close'
  where
    start' = endpoint @"start" $ \nfts -> startSale nfts
    buy'   = endpoint @"buy"   $ \nfts -> buy nfts
    close' = endpoint @"close" $ \nfts -> close nfts