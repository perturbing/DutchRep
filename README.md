# DutchRep
A simple plutus script for the basis of the Dutch Drep (more on this consortium later).

The Drep has hash `199ad2959c8c4e4d50a04a0f3d873b692ff86fbc6a195dd44d17b746` on mainnet (this [transaction](https://cexplorer.io/tx/cb177a185ab0629134ebcd0b0c94ee876f0afc0397a2a915b9fa8b1b14757d97/script#data) registered it). The Drep fingerprint is:
```bash
drep_script1rxdd99vu338y659qfg8nmpemdyhlsmaudgv4m4zdz7m5vz8uzt6
```
which can be deduced via a bech32 transformation of our script hash,
```bash
bech32 drep_script <<< 199ad2959c8c4e4d50a04a0f3d873b692ff86fbc6a195dd44d17b746\
```
Also see [this](https://www.1694.io/en/dreps/drep1rxdd99vu338y659qfg8nmpemdyhlsmaudgv4m4zdz7m5vd9p2hm) website for more info and to cross-reference the above claims. 

## Build the scripts yourself (via nix)
To reproduce the scripts in the asset folder (the NFT policy and the Drep script credential), you can do
```bash
nix run .#write-scripts
```
