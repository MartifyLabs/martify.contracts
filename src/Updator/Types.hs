{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Updator.Types
    ( UpdateVHash (..)
    , MarketParams' (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   ()

import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Eq(..) )
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash, ValidatorHash )


data MarketParams' = MarketParams'
    { feeAddrCli  :: !PubKeyHash
    , feeAddrNami :: !PubKeyHash
    , updateTn'   :: !TokenName
    , updateCs'   :: !CurrencySymbol
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''MarketParams' [('MarketParams', 0)]
PlutusTx.makeLift ''MarketParams'


newtype UpdateVHash = UpdateVHash
    { vhash :: ValidatorHash
    } deriving (Generic, ToJSON, FromJSON)

instance Eq UpdateVHash where
    {-# INLINABLE (==) #-}
    a == b = vhash a == vhash b

PlutusTx.makeIsDataIndexed ''UpdateVHash [('UpdateVHash, 0)]
PlutusTx.makeLift ''UpdateVHash
