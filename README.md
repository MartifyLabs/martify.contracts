# Martify
NFT Marketplace on the Cardano Blockchain. Powered by Plutus Smart Contracts
A simple contract that allows sales of NFTs. The seller (owner of an NFT) constructs the contract with parameters unique to the sale and locks the NFT there.
A buyer can then unlock the NFT by submitting a transaction verifying the several requirements defined in the validator.
***
### Test in emulator
* Clone the repository
* Run `cabal repl` in the repo
* Run `:l src/Market/Trace.hs` in the repl
* Run `test`
* To modify the testing scenario, open and modify the `src/Market/Trace.hs` file
***
### Test with `cardano-cli`
#### Put NFT for sale
* Install and run a `cardano-node` on the testnet, you'll now have a node.socket file. Put the path to this file in the env variable `CARDANO_NODE_SOCKET_PATH`
* Clone this repo https://github.com/nateshmbhat/cardano-nft-minter. It's an easy way to mint an NFT, so you can test and put it for sale
* Execute the bash script and mint an NFT, selecting testnet
* run `testnet=testnet-magic 1097911063`
* run `cd tokens/TokenName` after you minted the NFT
* run `cardano-cli query utxo --address $(cat payment.addr) --$testnet`
* Note down the txhash and txid containing the NFT you minted
* run `cardano-cli address key-hash --payment-verification-key-file payment.vkey` and copy the output
* Go into the Martify repo after you built it using `cabal build`
* run `cabal run market-plutus`
* copy the resulting `market.plutus` file in the folder where you minted the NFT
* run `cabal run datum-json sellerAddr price tokenName policyId royaltyAddr royaltyPercent` where sellerAddr is the output you copied previously, royaltyAddr should also be hash address (if you don't know, just put the same as sellerAddr and 0 for royaltyPercent)
* copy the resulting file in the folder where you minted the NFT
* go back to the folder where you minted the NFT
* run `cardano-cli address build --payment-script-file market.plutus --$testnet >> script.addr`
* run `cardano-cli transaction hash-script-data --script-data-file datum-policyId-tokenname.json >> dhash`
* run 
```
cardano-cli transaction build \
    --alonzo-era \
    --$testnet \
    --tx-in txhash#txid \
    --tx-out "$(cat script.addr) + 1724100 lovelace + 1 $(cat policy/policyID).tokenname" \
    --tx-out-datum-hash $(cat dhash) \
    --change-address $(cat payment.addr) \
    --protocol-params-file protocol.json \
    --out-file tx.02 
```
* run 
```
cardano-cli transaction sign \
    --tx-body-file tx.02 \
    --signing-key-file payment.skey \
    --$testnet \
    --out-file tx-2.02 
```
* run `cardano-cli transaction submit --tx-file tx-2.02 --$testnet`

#### Cancel sale (adapt for other endpoints buy and update price)
* run `cardano-cli query utxo --address $(cat script.addr) --$testnet`
* Get the txhash and txid of your NFT for sale. We'll call those txhashNFT and txidNFT
* run `cardano-cli query utxo --address $(cat payment.addr) --$testnet`
* Get the txhash and txid of some ada at your wallet. We'll call those txhashADA and txidADA
* Go into martify repo folder and copy over the close.json file from there
* run
```
cardano-cli transaction build \
    --alonzo-era \
    --$testnet \
    --tx-in txhashADA#txidADA \
    --tx-in txhashNFT#txidNFT \
    --tx-in-script-file market.plutus \
    --tx-in-datum-file datum-policyid-tokenname.json \
    --tx-in-redeemer-file close.json \
    --required-signer payment.skey \
    --tx-in-collateral txhashADA#txidADA \
    --tx-out "$(cat payment.addr) + 1724100 lovelace + 1 policyid.tokenname" \
    --change-address $(cat payment.addr) \
    --protocol-params-file protocol.json \
    --out-file unlock-body.02
```
* run
```
cardano-cli transaction sign \
    --tx-body-file unlock-body.02 \
    --signing-key-file payment.skey \
    --$testnet \
    --out-file unlock.02
```
* run `cardano-cli transaction submit --tx-file unlock.02 --$testnet`
* If everything went fine, you should see your asset in your wallet

## Donations
We would really appreciate financial support to our project, it helps us spend more time and effort on it.

ADA Address : addr1vycmju7zumcgq5wsdmdwkxafhhuu3xeed60auwngww2fkjcr38qjm

## Open source usage
Martify is an open source smart-contract for NFT Marketplaces.
The list of marketplaces based on our contracts is :
* https://jpg.store

