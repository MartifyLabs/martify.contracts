{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import System.Environment ( getArgs )
import Prelude
import Data.String (fromString)

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx

import Market.Types (NFTSale(..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for NFTSale custom Datum Type.
-- To use this enter `cabal run datum-json <price> <seller> <tn> <cs>`.
-- The <seller> needs to be in different format than the one usually used.
-- When the marketplace is released, we will update this section with instructions on how to convert to the required format.

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file in cardano-cli
main :: IO ()
main = do
  [price', seller', tn' ,cs', raddr', rprct'] <- getArgs
  let price  = read price'
      seller = fromString seller'
      tn     = fromString tn'
      cs     = fromString cs'
      raddr  = fromString raddr'
      rprct  = read rprct'
      nfts   = NFTSale seller price cs tn raddr rprct
  writeData ("datum-" ++ show cs ++ "-" ++ tn' ++ ".json") nfts
  putStrLn "Done"
-- Datum also needs to be passed when sending the token to the script (aka putting for sale)
-- When doing this, the datum needs to be hashed, see Alonzo-purple exercise-solutions on how to hash a datum

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
