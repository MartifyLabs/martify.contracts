{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Ledger (pubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, walletPubKey, fromWalletNumber, WalletNumber(..))
import           PlutusTx.Prelude ((.))

import           Prelude hiding ((.))

wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
