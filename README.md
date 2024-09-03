# DutchRep
A simple plutus script for the basis of the Dutch Drep (more on this consortium later).

The Drep has hash `e5ab37261b3d63600d566564879370aea031ea3108b0a6bd8cef58aa` on mainnet (this [transaction](https://cexplorer.io/tx/a37a8fd3d2bc6d92e7d9e370f70e106dfc06cb22cf081192bc7bfafcdf73c2a8/script#data) registered it). The Drep fingerprint is:
```bash
drep1uk4nwfsm843kqr2kv4jg0yms46srr633pzc2d0vvaav25ncwj8g
```
Also see [this](https://www.1694.io/en/dreps/drep1uk4nwfsm843kqr2kv4jg0yms46srr633pzc2d0vvaav25ncwj8g) website for more info and to cross-reference the above claims. 

## Build the scripts yourself (via nix)
To reproduce the scripts in the asset folder (the NFT policy and the Drep script credential), you can do
```bash
nix run .#write-scripts
```
