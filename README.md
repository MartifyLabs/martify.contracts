# Martify
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
* Run `cabal run market-plutus`

## Donations
We would really appreciate financial support to our project, it helps us spend more time and effort on it.

ADA Address : addr1vycmju7zumcgq5wsdmdwkxafhhuu3xeed60auwngww2fkjcr38qjm
