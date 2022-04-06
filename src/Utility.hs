{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet
    , companyPkh
    , companyPkhGhostsMainnet
    , mp
    , ghostMp ) where

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



-- Ghostchain params

companyPkhGhostsMainnet :: PubKeyHash
companyPkhGhostsMainnet = "ffdb13373ff4a2ca98657a357176967b4533799f74607b9bf31ed194"


ghostMp :: MarketParams
ghostMp = MarketParams companyPkhGhostsMainnet
