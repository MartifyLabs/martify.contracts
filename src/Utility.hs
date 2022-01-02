{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet
    , companyPkh
    , companyPkhReal
    , mp
    , mpReal
    , mpMainnet ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           Prelude hiding ((.))

import Market.Types  (MarketParams(..))

wallet :: Integer -> Wallet
wallet = knownWallet

companyPkh :: PubKeyHash
companyPkh = walletPubKeyHash $ wallet 1


mp :: MarketParams
mp = MarketParams companyPkh



companyPkhReal :: PubKeyHash
companyPkhReal = "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347"


mpReal :: MarketParams
mpReal = MarketParams companyPkhReal




companyPkhMainnet :: PubKeyHash
companyPkhMainnet = "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347"


mpMainnet :: MarketParams
mpMainnet = MarketParams companyPkhMainnet