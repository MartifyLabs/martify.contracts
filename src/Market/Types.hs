{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Market.Types
    ( NFTSale (..)
    , SaleAction (..)
    , SaleSchema
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..))
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Eq(..), (&&), Integer )
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash )
import           Plutus.Contract           ( Endpoint, type (.\/) )


data NFTSale = NFTSale
    { nSeller    :: !PubKeyHash
    , nPrice     :: !Integer
    , nCurrency  :: !CurrencySymbol
    , nToken     :: !TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTSale where
    {-# INLINABLE (==) #-}
    a == b = (nSeller    a == nSeller    b) &&
             (nPrice     a == nPrice     b) &&
             (nCurrency  a == nCurrency  b) &&
             (nToken     a == nToken     b)

PlutusTx.makeIsDataIndexed ''NFTSale [('NFTSale, 0)]
PlutusTx.makeLift ''NFTSale


data SaleAction = Buy | Close
    deriving Show

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0), ('Close, 1)]
PlutusTx.makeLift ''SaleAction


type SaleSchema = Endpoint "close" NFTSale .\/ Endpoint "buy" NFTSale .\/ Endpoint "start" NFTSale