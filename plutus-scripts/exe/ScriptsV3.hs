{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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

module ScriptsV3 (
    dutchDrepCredentialCode,
    dutchDrepLockScriptCode,
    -- Testing purposes
    alwaysTrueCodeV3,
) where

import PlutusLedgerApi.V3 (
    CurrencySymbol,
    Datum (..),
    FromData (..),
    OutputDatum (..),
    PubKeyHash,
    Redeemer (..),
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
    (<$>),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

-- [General notes on this file]
-- This file contains two plutus scripts, the script that will be used as the Dutch DREP,
-- called 'dutchDrepCredential'. The other script is a spending script called `dutchDrepLockScript`.
--
-- The `dutchDrepCredential` script is parametrized by the currency symbol of the NFT that will be locked
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

-- [The Dutch Drep lock script actions ]
data LockScriptActions = Vote | Update | Recover
makeIsDataIndexed ''LockScriptActions [('Vote, 0), ('Update, 1), ('Recover, 2)]

-- [The Dutch Drep lock script datum]
data LockScriptDatum = One | Two | Three
makeIsDataIndexed ''LockScriptDatum [('One, 0), ('Two, 1), ('Three, 2)]

-- [The Dutch Drep lock script]
{-# INLINEABLE dutchDrepLockScript #-}
dutchDrepLockScript :: ScriptContext -> Bool
dutchDrepLockScript ctx = case scriptContextScriptInfo ctx of
    SpendingScript txOutRef maybeDtm -> case fromBuiltinData @LockScriptDatum . getDatum <$> maybeDtm of
        Just dtm -> False -- TODO: Implement the Dutch DREP lock script
        Nothing -> True
    _ -> False
  where
    maybeRed = fromBuiltinData @LockScriptActions . getRedeemer $ scriptContextRedeemer ctx

{-# INLINEABLE wrappedDutchDrepLockScript #-}
wrappedDutchDrepLockScript :: BuiltinData -> BuiltinUnit
wrappedDutchDrepLockScript = wrapOneArg dutchDrepLockScript

dutchDrepLockScriptCode :: CompiledCode (BuiltinData -> BuiltinUnit)
dutchDrepLockScriptCode = $$(compile [||wrappedDutchDrepLockScript||])

-- Testing purposes

{-# INLINEABLE alwaysTrue #-}
alwaysTrue :: BuiltinData -> Bool
alwaysTrue _ = True

{-# INLINEABLE wrappedAlwaysTrue #-}
wrappedAlwaysTrue :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrue = wrapOneArg alwaysTrue

alwaysTrueCodeV3 :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueCodeV3 = $$(compile [||wrappedAlwaysTrue||])
