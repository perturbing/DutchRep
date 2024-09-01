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
    alwaysTrueMintCode,
) where

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
    map,
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

-- [General notes on this file]
-- This file contains three plutus scripts, the script that will be used as the Dutch DREP,
-- called 'dutchDrepCredential'. The other script is a spending script called `dutchDrepLockScript` and
-- a simple NFT minting script that is made unique by parametrizing a UTXO that needs to be spend.
--
-- The `dutchDrepLockScript` script is parameterized by the currency symbol of the NFT that will be locked
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

-- Testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint = wrapOneArg alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
