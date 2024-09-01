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

module Scripts (
    dutchDrepCredentialCode,
    dutchDrepLockScriptCode,
    dutchDrepNFTScriptCode,
    -- Testing purposes
    alwaysTrueCode,
) where

import PlutusLedgerApi.V1.Data.Value (flattenValue)
import PlutusLedgerApi.V3 (
    CurrencySymbol,
    Datum (..),
    OutputDatum (..),
    PubKeyHash,
    ScriptContext (..),
    ScriptInfo (..),
    ScriptPurpose (..),
    ToData (..),
    TxCert (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
    UnsafeFromData (..),
    Value (..),
 )
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
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

-- [General notes on this file]
-- This file contains three plutus scripts, the script that will be used as the Dutch DREP,
-- called 'dutchDrepCredential'. The other script is a spending script called `dutchDrepLockScript` and
-- a simple NFT minting script that is made unique by parametrizing a UTXO that needs to be spent.
--
-- The `dutchDrepLockScript` script is parametrized by the currency symbol of the NFT that will be locked
-- at the `dutchDrepLockScript` script. The `dutchDrepLockScript` script defines under what conditions the
-- NFT can witness a certificate related to the Dutch DREP (e.g. register / update / vote as the DREP).
-- For now only the `dutchDrepCredential` script is implemented, the `dutchDrepLockScript` script will be
-- implemented in the future.

-- [The Dutch Drep credential script]
-- This script just checks that the hard-coded currency symbol of the NFT is
-- in any spending input of the transaction. Given that unique NFT the full
-- control of the voting rights of the Dutch DREP.
{-# INLINEABLE dutchDrepCredential #-}
dutchDrepCredential :: CurrencySymbol -> ScriptContext -> Bool
dutchDrepCredential symbol ctx = case scriptContextScriptInfo ctx of
    CertifyingScript _ _ -> any (\value -> symbol `member` value) txInputsValues
    _ -> False
  where
    -- The list of transaction inputs being consumed in this transaction.
    txInputs = txInfoInputs . scriptContextTxInfo $ ctx
    -- The list of value maps of the transaction inputs.
    txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

{-# INLINEABLE wrappedDutchDrepCredential #-}
wrappedDutchDrepCredential :: BuiltinData -> BuiltinData -> BuiltinUnit
wrappedDutchDrepCredential = wrapTwoArgs dutchDrepCredential

dutchDrepCredentialCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
dutchDrepCredentialCode = $$(compile [||wrappedDutchDrepCredential||])

-- [The Dutch Drep lock script]
-- This script is not implemented yet.
{-# INLINEABLE dutchDrepLockScript #-}
dutchDrepLockScript :: ScriptContext -> Bool
dutchDrepLockScript _ = True

{-# INLINEABLE wrappedDutchDrepLockScript #-}
wrappedDutchDrepLockScript :: BuiltinData -> BuiltinUnit
wrappedDutchDrepLockScript = wrapOneArg dutchDrepLockScript

dutchDrepLockScriptCode :: CompiledCode (BuiltinData -> BuiltinUnit)
dutchDrepLockScriptCode = $$(compile [||wrappedDutchDrepLockScript||])

-- [The NFT minting script]
-- This script is a simple minting script that will mint a unique NFT by parametrizing
-- the UTXO that needs to be spent. Note that this NFT cannot be burned, this is
-- deliberate to ensure assumptions about the NFT.
{-# INLINEABLE dutchDrepNFTScript #-}
dutchDrepNFTScript :: TxOutRef -> ScriptContext -> Bool
dutchDrepNFTScript utxo ctx = case scriptContextScriptInfo ctx of
    MintingScript sym -> consumesUTxO && checkMintedValue sym
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
            let ((tn, n) : xs) = toList value
             in null xs && n == 1
        Nothing -> False

{-# INLINEABLE wrappedDutchDrepNFTScript #-}
wrappedDutchDrepNFTScript :: BuiltinData -> BuiltinData -> BuiltinUnit
wrappedDutchDrepNFTScript = wrapTwoArgs dutchDrepNFTScript

dutchDrepNFTScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
dutchDrepNFTScriptCode = $$(compile [||wrappedDutchDrepNFTScript||])

-- Testing purposes

{-# INLINEABLE alwaysTrue #-}
alwaysTrue :: BuiltinData -> Bool
alwaysTrue _ = True

{-# INLINEABLE wrappedAlwaysTrue #-}
wrappedAlwaysTrue :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrue = wrapOneArg alwaysTrue

alwaysTrueCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueCode = $$(compile [||wrappedAlwaysTrue||])
