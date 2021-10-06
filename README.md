# Vendere
NFT Marketplace on the Cardano Blockchain. Powered by Plutus Smart Contracts

# Test in emulator
* Clone the repository
* Run `cabal repl` in the repo
* Run `:l src/Market/Trace.hs` in the repl
* Run `test`
* To modify the testing scenario, open and modify the `src/Market/Trace.hs` file

### To compile to .plutus code
* Run `cabal run market-plutus <seller's address (addr1...)> <price> <TokenName> <CurrencySymbol>`
* For example `cabal run market-plutus addr1vyzftlax285dex3ggs54cpqe08efgs9n2n92mts09h3slwsrvwg9h 34 Vendere 66`
