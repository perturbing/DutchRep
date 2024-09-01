# DutchRep
A simple plutus script for the basis of the dutch DREP.

The Drep has hash `e5ab37261b3d63600d566564879370aea031ea3108b0a6bd8cef58aa` on mainnet.

# instructions to launch it
### create NFT
```bash
cardano-cli transaction policyid --script-file dutchDrepNFT.plutus > dutchDrepNFT.hash
```
then
```bash
cardano-cli transaction build --mainnet \
 --tx-in "$(cardano-cli query utxo --address "$(cat payment.addr)" --mainnet --out-file /dev/stdout | jq -r 'keys[0]')" \
 --tx-in-collateral "$(cardano-cli query utxo --address "$(cat payment.addr)" --mainnet --out-file /dev/stdout | jq -r 'keys[0]')" \
 --tx-out $(cat payment.addr)+5000000+"1 $(cat dutchDrepNFT.hash).44757463682044524550" \
 --mint "1 $(cat dutchDrepNFT.hash).44757463682044524550" \
 --change-address $(cat payment.addr) \
 --mint-script-file dutchDrepNFT.plutus --mint-redeemer-value {} \
 --out-file tx.raw
cardano-cli transaction sign --mainnet \
 --signing-key-file payment.skey \
 --tx-body-file tx.raw \
 --out-file tx.signed
cardano-cli transaction submit --mainnet --tx-file tx.signed
```
### register drep
Create and register the drep
```bash
cardano-cli transaction policyid --script-file dutchDrepCredential.plutus > dutchDrepCredential.hash
```
then (after HF)
```bash
cardano-cli conway governance drep registration-certificate \
 --drep-script-hash $(cat dutchDrepCredential.hash) \
 --key-reg-deposit-amt $(cardano-cli conway query gov-state --mainnet | jq -r .currentPParams.dRepDeposit) \
 --out-file register-dutchDrepCredential.
```
create tx
```bash
cardano-cli conway transaction build --mainnet \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --mainnet | jq -r 'keys[0]') \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --mainnet | jq -r 'keys[1]') \
 --tx-in-collateral $(cardano-cli query utxo --address $(cat payment.addr) --output-json --mainnet | jq -r 'keys[2]') \
 --tx-out $(cat payment.addr)+5000000+"1 $(cat dutchDrepNFT.hash).44757463682044524550" \
 --certificate-file register-dutchDrepCredential.cert \
 --certificate-script-file dutchDrepCredential.plutus \
 --certificate-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx
cardano-cli transaction sign --mainnet --signing-key-file payment.skey --tx-body-file tx --out-file tx.signed
cardano-cli transaction submit --mainnet --tx-file tx.signed
```
and check success via
```bash
cardano-cli conway query drep-state --mainnet --all-dreps | grep $(cat dutchDrepCredential.hash)
```