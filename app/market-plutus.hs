{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Cardano.Api ( writeFileTextEnvelope, Error(displayError) )

import Prelude

import Market.Onchain (apiBuyScript)


main :: IO ()
main = do
    let marketFile = "scripts/market.plutus"
    marketResult <- writeFileTextEnvelope marketFile Nothing apiBuyScript
    case marketResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote market script to file " ++ marketFile