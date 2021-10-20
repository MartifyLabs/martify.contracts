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
* For example `cabal run market-plutus addr1vyzftlax285dex3ggs54cpqe08efgs9n2n92mts09h3slwsrvwg9h 34 Vendere 66`
