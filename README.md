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

### To test against a validator
#### Do the following only the first time
* Start a testnet node a note where is the `node.socket` file
* `CARDANO_NODE_SOCKET_PATH=/absolute/path/to/node.socket`
* `testnet="testnet-magic 1097911063"`
* Create a folder for wallet 1 `w1` the seller and for wallet 2 `w2` the buyer
* Generate payment files in **both** folders :
* * `cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey`
* * `cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr --$testnet`
* Export protocol params in **both** folders `cardano-cli query protocol-parameters --$testnet --out-file protocol.json`
* I suggest downloading Daedalus testnet [Daedalus-testnet](https://testnets.cardano.org/en/testnets/cardano/get-started/wallet/)
* Fund the new Daedalus wallet using the testnet faucet at [faucet](https://developers.cardano.org/docs/integrate-cardano/testnet-faucet)
* From daedalus, fund the two cli addresses (`w1` and `w2`) (with the faucet you should get 100ADA so fund w1 with 500 and w2 with 500)
##### Now we have to mint some tokens (only in w1) that will be the NFTs we sell to test the validator
* Go in `w1` folder and execute `address=$(cat payment.addr)`
* Run `cardano-cli query utxo --address $address --$testnet`
* `txhashAda=< here put the txhash#txid that contains a lot of ada >` example : `txhashAda=8d866294eb8ff438858d4e825ee266a6c187f62d35c10ffc3ee1aa0f661e1397#1`
* `funds=< here put the funds sitting at the utxo hash you just used above (exact number) >`
* `fee=3000000`
* `output=0`
* `mkdir policy`
* `cardano-cli address key-gen --verification-key-file policy/policy.vkey --signing-key-file policy/policy.skey`
* `touch policy/policy.script && echo "" > policy/policy.script`
* `echo "{" >> policy/policy.script`
* `echo "  \"keyHash\": \"$(cardano-cli address key-hash --payment-verification-key-file policy/policy.vkey)\"," >> policy/policy.script`
* `echo "  \"type\": \"sig\"" >> policy/policy.script`
* `echo "}" >> policy/policy.script`
* `cardano-cli transaction policyid --script-file ./policy/policy.script >> policy/policyID`
* `policyid=$(cat policy/policyID)`
* `cardano-cli transaction build-raw --fee $fee --tx-in $txhashAda --tx-out $address+$output+"100 $policyid.Vendere" --mint="100 $policyid.Vendere" --minting-script-file policy/policy.script --out-file matx.raw` this will mint 100 Vendere tokens
* `fee=$(cardano-cli transaction calculate-min-fee --tx-body-file matx.raw --tx-in-count 1 --tx-out-count 1 --witness-count 1 --$testnet --protocol-params-file protocol.json | cut -d " " -f1)`
* `output=$(expr $funds - $fee)`
* `cardano-cli transaction build-raw --fee $fee --tx-in $txhashAda --tx-out $address+$output+"100 $policyid.Vendere" --mint="100 $policyid.Vendere" --minting-script-file policy/policy.script --out-file matx.raw`
* `cardano-cli transaction sign --signing-key-file payment.skey --signing-key-file policy/policy.skey --$testnet --tx-body-file matx.raw --out-file matx.signed`
* `cardano-cli transaction submit --tx-file matx.signed --$testnet`
* `cardano-cli query utxo --address $address --$testnet`
* Now you should have 100 Vendere tokens at your w1 wallet (it can take some time to appear). We'll be using those to test our validator

#### Do the following every time you test after completing initial setup above
##### Send NFT to script
* Start a testnet node and see where is `node.socket`
* `CARDANO_NODE_SOCKET_PATH=/absolute/path/to/node.socket`
* `testnet="testnet-magic 1097911063"`
* Go into the project folder and compile to plutus code with right parameters (tokenname should be `Vendere`, the right policy id, the right seller, the price can be arbitrary but remember what you chose)
* Copy the `market.plutus` file to `w1` folder
* Go in `w1` folder
* Run `cardano-cli address build --payment-script-file market.plutus --$testnet >> script.addr`
* `address=$(cat payment.addr)`
* Run `cardano-cli query utxo --address $address --$testnet`
* `txhashAda=< here put the txhash#txid that contains a lot of ada >`
* `txhashNFT=< here put the txhash#txid that contains the NFT you want to sell, can be same as $txhashAda >`
* Build the tx
```
cardano-cli transaction build \
    --alonzo-era \
    --$testnet \
    --tx-in $txhashAda \
    --tx-in $txhashNFT \
    --tx-in-collateral $txhashAda \
    --tx-out-datum-hash 45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0 \
    --tx-out "$(cat script.addr) + 1724100 lovelace + 1 $policyid.Vendere" \
    --tx-out "$address + 1724100 lovelace + <amount of vendere tokens left> $policyid.Vendere" \
    --change-address $address \
    --protocol-params-file protocol.json \
    --out-file tx.02
```
* Sign the tx
```
cardano-cli transaction sign \
    --tx-body-file tx.02 \
    --signing-key-file payment.skey \
    --$testnet \
    --out-file tx-2.02
```
* Submit the tx : `cardano-cli transaction submit --tx-file tx-2.02 --$testnet`
* Check the balance in the script, you should see the NFT : `cardano-cli query utxo --address $(cat script.addr) --$testnet`

##### Attempt to unlock NFT
* I suggest opening a different terminal
* Copy the `market.plutus` file used previously to `w2` folder
* Go in `w2` folder
* `CARDANO_NODE_SOCKET_PATH=/absolute/path/to/node.socket`
* `testnet="testnet-magic 1097911063"`
* `address=$(cat payment.addr)`
* `cardano-cli query utxo --address $(cat script.addr) --$testnet`
* `txhash=< here put the txhash#txid of the NFT locked at the script addr >`
* `cardano-cli query utxo --address $address --$testnet`
* `txhashw2=< put a txhash#txid of w2 that contains ada >` this is used as collateral, to pay fees
* `w2addr=< here put the addr of w2 >`
* `w1addr=< here put the addr of w1 >`
* `keyfile=payment.skey`
* Build the tx :
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in $txhashw2 \
    --tx-in $txhash \
    --tx-in-script-file market.plutus \
    --tx-in-datum-file datum.json \
    --tx-in-redeemer-file redeemer.json \
    --tx-in-collateral $txhashw2 \
    --tx-out "$w2addr + $value" \
    --tx-out "$w1addr + 1000000 lovelace" \
    --change-address $w2addr \
    --protocol-params-file protocol.json \
    --out-file unlock-body.02
``` 
* If there are any validation errors they should pop here. If the build was successful (the ouput should show an estimate of fees) then move on
* Sign the tx :
```
cardano-cli transaction sign \
    --tx-body-file unlock-body.02 \
    --signing-key-file payment.skey \
    --$testnet \
    --out-file unlock.02
```
* This step should never fail, move on
* Submit the tx : `cardano-cli transaction submit --tx-file unlock.02 --$testnet` this is where the current error pops. Good luck :) (if you encounter any errors or don't understand something, please ask on discord)