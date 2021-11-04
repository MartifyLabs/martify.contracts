{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet
    , companyPkh
    , companyPkhReal ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Ledger (pubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, walletPubKey, fromWalletNumber, WalletNumber(..))
import           PlutusTx.Prelude ((.))

import           Prelude hiding ((.))

wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey

companyPkh :: PubKeyHash
companyPkh = walletPubKeyHash $ wallet 1

companyPkhReal :: PubKeyHash
companyPkhReal = "a75c75fa79bc7d53ef715d64745a7a01c2c1f7653b2ae962413ac521"
