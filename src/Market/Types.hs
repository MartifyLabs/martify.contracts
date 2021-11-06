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
    , StartParams (..)
    , BuyParams (..)
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

-- This is the datum type, carrying the previous validator params
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


data SaleAction = Buy | Update | Close
    deriving Show

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0), ('Update, 1), ('Close, 2)]
PlutusTx.makeLift ''SaleAction


-- We define two different params for the two endpoints start and buy with the minimal info needed.
-- Therefore the user doesn't have to provide more that what's needed to execute the said action.
{- For StartParams we ommit the seller
    because we automatically input the address of the wallet running the startSale enpoint
    
   For BuyParams we ommit seller and price
    because we can read that in datum which can be obtained with just cs and tn of the sold token -}

data BuyParams = BuyParams
    { bCs :: CurrencySymbol
    , bTn :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


data StartParams = StartParams
    { sPrice :: Integer
    , sCs    :: CurrencySymbol
    , sTn    :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


type SaleSchema = Endpoint "close" BuyParams 
                  .\/
                  Endpoint "buy" BuyParams
                  .\/
                  Endpoint "update" (BuyParams, Integer)
                  .\/
                  Endpoint "start" StartParams