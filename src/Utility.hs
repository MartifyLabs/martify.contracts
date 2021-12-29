{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet
    , companyPkh
    , companyPkhReal
    , mp
    , mp'
    , mpReal
    , mpReal'
    , mpMainnet ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Ledger (TokenName, CurrencySymbol)
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           Prelude hiding ((.))

import Market.Types  (MarketParams(..))
import Updator.Types (MarketParams'(..))

wallet :: Integer -> Wallet
wallet = knownWallet

companyPkh :: PubKeyHash
companyPkh = walletPubKeyHash $ wallet 1

companyPkh' :: PubKeyHash
companyPkh' = walletPubKeyHash $ wallet 6

uTn :: TokenName
uTn = "UpdateToken"

uCs :: CurrencySymbol
uCs = "9236a326ec65243627d89f60921a42314d0cd407c002280499e1f88b"

mp :: MarketParams
mp = MarketParams companyPkh uTn uCs

mp' :: MarketParams'
mp' = MarketParams' companyPkh companyPkh' uTn uCs

companyPkhReal :: PubKeyHash
companyPkhReal = "74125b187d91d0495e14648ac24bf7b470c7d400ce0a8a29a99bb4c4"

uCsReal :: CurrencySymbol
uCsReal = "1bee72f6551a6f0cccc67d05c4b8652755160f2d35a5f6d64f3c75b8"

uTnReal :: TokenName
uTnReal = "UpdateToken"

mpReal :: MarketParams
mpReal = MarketParams companyPkhReal uTnReal uCsReal

mpReal' :: MarketParams'
mpReal' = MarketParams' companyPkhReal companyPkhReal uTnReal uCsReal


companyPkhMainnet :: PubKeyHash
companyPkhMainnet = "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347"

uCsMainnet :: CurrencySymbol
uCsMainnet = "4d6397abc7ebe681c482791c0d897b04a74158b77a943867f4c334bb"

uTnMainnet :: TokenName
uTnMainnet = "557064617465546f6b656e4d617274696679"

mpMainnet :: MarketParams
mpMainnet = MarketParams companyPkhMainnet uTnMainnet uCsMainnet