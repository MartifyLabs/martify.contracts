{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Cardano.Api ( writeFileTextEnvelope, Error(displayError) )
import Ledger      ( PubKeyHash(PubKeyHash) )
import PlutusTx.Builtins.Class ( ToBuiltin(toBuiltin) )

import Data.String (IsString (..))
import Prelude
import System.Environment                  (getArgs)
import Codec.Binary.Bech32 ( dataPartToBytes, decode )


import Market.Onchain (apiBuyScript)
import Market.Types   (NFTSale(..))

main :: IO ()
main = do
    [nftSeller, nftPrice, nftName, nftSymbol] <- getArgs
    let Right (_, dataPart) = decode $ fromString nftSeller
    case dataPartToBytes dataPart of
        Nothing    -> print ("error" :: [Char])
        Just pkhBS -> do
            let nfts = NFTSale
                        { nSeller   = PubKeyHash $ toBuiltin pkhBS
                        , nPrice    = read nftPrice
                        , nToken    = fromString nftName
                        , nCurrency = fromString nftSymbol
                        }
                marketFile = "scripts/market.plutus"
            print nfts
            marketResult <- writeFileTextEnvelope marketFile Nothing $ apiBuyScript nfts
            case marketResult of
                Left err -> print $ displayError err
                Right () -> putStrLn $ "wrote market script to file " ++ marketFile