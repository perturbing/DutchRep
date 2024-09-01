{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
-- This pragma is required to use the target-version option for writing old plutusV2 scripts
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module ScriptsV2 (
    dutchDrepNFTScriptCode,
    -- Testing purposes
    alwaysTrueCodeV2,
) where

import PlutusLedgerApi.V2 (
    ScriptContext (..),
    ScriptPurpose (..),
    TxInInfo (..),
    UnsafeFromData (..),
    Value (..),
 )
import PlutusLedgerApi.V2.Contexts (TxInfo (..))
import PlutusLedgerApi.V2.Tx (TxOutRef)
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.AssocMap (
    all,
    lookup,
    member,
    toList,
 )
import PlutusTx.Bool (
    Bool (..),
    otherwise,
    (&&),
 )
import PlutusTx.Builtins (
    BuiltinByteString,
    BuiltinData,
    Integer,
    error,
 )
import PlutusTx.Prelude (
    BuiltinUnit,
    Maybe (..),
    any,
    find,
    length,
    map,
    null,
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgsV2, wrapOneArgV2, wrapThreeArgsV2, wrapTwoArgsV2)

-- [The NFT minting script]
-- This script is a simple minting script that will mint a unique NFT by parametrizing
-- the UTXO that needs to be spent. Note that this NFT cannot be burned, this is
-- deliberate to ensure assumptions about the NFT.
{-# INLINEABLE dutchDrepNFTScript #-}
dutchDrepNFTScript :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
dutchDrepNFTScript utxo _ ctx = case scriptContextPurpose ctx of
    Minting sym -> consumesUTxO && checkMintedValue sym
    _ -> False
  where
    -- The list of transaction inputs being consumed in this transaction.
    txInputs = txInfoInputs . scriptContextTxInfo $ ctx
    -- Check if the parametrized UTXO is being consumed in this transaction.
    consumesUTxO = any (\txIn -> txInInfoOutRef txIn == utxo) txInputs
    -- The minted value map in this transaction.
    mintedValueMap = getValue . txInfoMint . scriptContextTxInfo $ ctx
    -- Check if the minted value is indeed that of a unique NFT.
    checkMintedValue sym' = case lookup sym' mintedValueMap of
        Just value ->
            let ((_tn, n) : xs) = toList value
             in null xs && n == 1
        Nothing -> False

{-# INLINEABLE wrappedDutchDrepNFTScript #-}
wrappedDutchDrepNFTScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedDutchDrepNFTScript = wrapThreeArgsV2 dutchDrepNFTScript

dutchDrepNFTScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
dutchDrepNFTScriptCode = $$(compile [||wrappedDutchDrepNFTScript||])

-- Testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> BuiltinData -> Bool
alwaysTrueMint _ _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinData -> ()
wrappedAlwaysTrueMint = wrapTwoArgsV2 alwaysTrueMint

alwaysTrueCodeV2 :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysTrueCodeV2 = $$(compile [||wrappedAlwaysTrueMint||])
