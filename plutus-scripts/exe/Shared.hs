{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Shared where

import qualified PlutusLedgerApi.V2 as PlutusV2
import PlutusLedgerApi.V3 (
    ScriptContext (..),
    UnsafeFromData (..),
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.Builtins (
    BuiltinData,
    error,
 )
import PlutusTx.Prelude (
    Bool (..),
    BuiltinUnit,
    check,
    ($),
 )

-- Helper function to wrap a script to error on the return of a False.
{-# INLINEABLE wrapOneArg #-}
wrapOneArg ::
    (UnsafeFromData a) =>
    (a -> Bool) ->
    (BuiltinData -> BuiltinUnit)
wrapOneArg f ctx =
    check
        $ f
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapTwoArgs #-}
wrapTwoArgs ::
    ( UnsafeFromData a
    , UnsafeFromData b
    ) =>
    (a -> b -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinUnit)
wrapTwoArgs f a ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapThreeArgs #-}
wrapThreeArgs ::
    ( UnsafeFromData a
    , UnsafeFromData b
    , UnsafeFromData c
    ) =>
    (a -> b -> c -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
wrapThreeArgs f a b ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapFourArgs #-}
wrapFourArgs ::
    ( UnsafeFromData a
    , UnsafeFromData b
    , UnsafeFromData c
    , UnsafeFromData d
    ) =>
    (a -> b -> c -> d -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
wrapFourArgs f a b c ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData c)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE checkV2 #-}
checkV2 :: Bool -> ()
checkV2 b = if b then () else error ()

{-# INLINEABLE wrapOneArgV2 #-}
wrapOneArgV2 ::
    (UnsafeFromData a) =>
    (a -> Bool) ->
    (BuiltinData -> ())
wrapOneArgV2 f ctx =
    checkV2
        $ f
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapTwoArgsV2 #-}
wrapTwoArgsV2 ::
    ( UnsafeFromData a
    , UnsafeFromData b
    ) =>
    (a -> b -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
wrapTwoArgsV2 f a ctx =
    checkV2
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapThreeArgsV2 #-}
wrapThreeArgsV2 ::
    ( UnsafeFromData a
    , UnsafeFromData b
    , UnsafeFromData c
    ) =>
    (a -> b -> c -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapThreeArgsV2 f a b ctx =
    checkV2
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapFourArgsV2 #-}
wrapFourArgsV2 ::
    ( UnsafeFromData a
    , UnsafeFromData b
    , UnsafeFromData c
    , UnsafeFromData d
    ) =>
    (a -> b -> c -> d -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapFourArgsV2 f a b c ctx =
    checkV2
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData c)
            (unsafeFromBuiltinData ctx)
