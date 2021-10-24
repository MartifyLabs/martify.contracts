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

import Utility         (wallet)
import Market.Offchain (endpoints)
import Market.Types    (StartParams(..), BuyParams(..))

nftEx1 :: StartParams
nftEx1 = StartParams
    { sPrice = 10_000_000
    , sTn    = "Vendere"
    , sCs    = "66"
    } -- This is an example token, 
      -- As these are the parameters of the validator, this info should be provided by the user of the contract

nftEx2 :: StartParams
nftEx2 = StartParams
    { sPrice = 10_000_000
    , sTn    = "Vendere2"
    , sCs    = "66"
    }

nftEx1' :: BuyParams
nftEx1' = BuyParams
    { bTn = sTn nftEx1
    , bCs = sCs nftEx1
    }

nftEx2' :: BuyParams
nftEx2' = BuyParams
    { bTn = sTn nftEx2
    , bCs = sCs nftEx2
    }


test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 100_000_000
                                      <> Value.singleton (sCs nftEx1) (sTn nftEx1) 1)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 100_000_000
                                      <> Value.singleton (sCs nftEx2) (sTn nftEx2) 1)
                            , (wallet 4, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 6, Ada.lovelaceValueOf 100_000_000)
                            ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (wallet 1) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        h4 <- activateContractWallet (wallet 4) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h1 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 nftEx1'
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h3 nftEx2
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 nftEx2'
        void $ Emulator.waitNSlots 1
    