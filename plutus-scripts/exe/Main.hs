{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Api (
    IsPlutusScriptLanguage (..),
    PlutusScriptV1,
    PlutusScriptV2,
    PlutusScriptV3,
    PlutusScriptVersion (..),
    Script (..),
    ScriptHash (..),
    hashScript,
    prettyPrintJSON,
    writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (
    File (..),
    PlutusScript (..),
    Script (..),
    fromPlutusData,
    scriptDataToJsonDetailedSchema,
    serialiseToRawBytes,
    unsafeHashableScriptData,
 )
import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusTx (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusTx.Builtins as PlutusTx

import Scripts (
    alwaysTrueCode,
    dutchDrepCredentialCode,
    dutchDrepLockScriptCode,
    dutchDrepNFTScriptCode,
 )

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import GHC.ByteOrder (ByteOrder (..))
import qualified PlutusTx.Prelude as P

writePlutusScriptToFile :: (IsPlutusScriptLanguage lang) => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script =
    writeFileTextEnvelope (File filePath) Nothing script >>= \case
        Left err -> print "error writing script"
        Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
    PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
    PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
    PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

dataToJSON :: (PlutusV3.ToData a) => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

printDataToJSON :: (PlutusV3.ToData a) => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

---------------------------------------

-- Define the TxOutRef that needs to be spent to mint the DutchDrep NFT
initTxOutRef :: PlutusV3.TxOutRef
initTxOutRef =
    PlutusV3.TxOutRef
        ((PlutusV3.TxId . P.integerToByteString BigEndian 32) 0x7f6a7f48d447b1ae63ec2b8b1a623cca4d36838f8010e545c565c2d26ccb70f5)
        1

-- The above TxOutRef is applied to the script to finalize the script
appliedDutchDrepNFTScriptCode :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
appliedDutchDrepNFTScriptCode = dutchDrepNFTScriptCode `unsafeApplyCode` PlutusTx.liftCodeDef (PlutusV3.toBuiltinData initTxOutRef)

scriptHashDutchDrepNFT :: ScriptHash
scriptHashDutchDrepNFT = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ appliedDutchDrepNFTScriptCode

-- The currency symbol of the DutchDrep NFT
dutchDrepNFTSymbol :: PlutusV3.CurrencySymbol
dutchDrepNFTSymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashDutchDrepNFT

-- The above currency symbol is applied to the script to finalize the script
appliedDutchDrepCredentialCode :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
appliedDutchDrepCredentialCode = dutchDrepCredentialCode `unsafeApplyCode` PlutusTx.liftCodeDef (PlutusV3.toBuiltinData dutchDrepNFTSymbol)

scriptHashDutchDrepCredential :: ScriptHash
scriptHashDutchDrepCredential = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ appliedDutchDrepCredentialCode

main :: IO ()
main = do
    -- writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrue.plutus" alwaysTrueCode
    writeCodeToFile PlutusScriptV3 "./assets/V3/dutchDrepNFT.plutus" appliedDutchDrepNFTScriptCode
    print $ "Currency Symbol (PolicyID) of the Dutch Drep NFT: " ++ show dutchDrepNFTSymbol
    writeCodeToFile PlutusScriptV3 "./assets/V3/dutchDrepCredential.plutus" appliedDutchDrepCredentialCode
    print $ "Script Hash of Credential: " ++ show scriptHashDutchDrepCredential
