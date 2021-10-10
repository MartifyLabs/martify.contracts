# Vendere
NFT Marketplace on the Cardano Blockchain. Powered by Plutus Smart Contracts
A simple contract that allows sales of NFTs. The seller (owner of an NFT) constructs the contract with parameters unique to the sale and locks the NFT there.
A buyer can then unlock the NFT by submitting a transaction verifying the several requirements defined in the validator.

### Test in emulator
* Clone the repository
* Run `cabal repl` in the repo
* Run `:l src/Market/Trace.hs` in the repl
* Run `test`
* To modify the testing scenario, open and modify the `src/Market/Trace.hs` file

### To compile to .plutus code
* Run `cabal run market-plutus <seller's address (addr1...)> <price> <TokenName> <CurrencySymbol>`
* For example 
  
  `cabal run market-plutus addr_test1vp95x4dv7hj5n4xdyeqgwg73erv84xxmwkvwgza3e5ang0sgz6rzm 34 Vendere 66`
* Note : the price is in Lovelace, not Ada (temporary)
