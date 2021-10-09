{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Trace
    ( test
    ) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..) )
import           Ledger.Value     as Value (singleton)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada

import           Prelude      (IO)
import           Data.Default (def)

import Utility         (wallet, walletPubKeyHash)
import Market.Offchain (endpoints)
import Market.Types    (NFTSale(..))

nftEx :: NFTSale
nftEx = NFTSale
    { nSeller   = walletPubKeyHash $ wallet 1
    , nPrice    = 10_000_000
    , nToken    = "Vendere"
    , nCurrency = "66"
    } -- This is an example token, 
      -- As these are the parameters of the validator, this info should be provided by the user of the contract


-- Scenario where w1 puts nftEx for sale and w2 buys it
test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 100_000_000
                                      <> Value.singleton (nCurrency nftEx) (nToken nftEx) 1)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h1 nftEx
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h2 nftEx
        void $ Emulator.waitNSlots 1
    