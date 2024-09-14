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
import qualified PlutusCore.Version as PLC
import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusTx (
    CompiledCode,
    liftCode,
    liftCodeDef,
    unsafeApplyCode,
 )
import qualified PlutusTx.Builtins as PlutusTx

import ScriptsV2 (
    alwaysTrueCodeV2,
    dutchDrepNFTScriptCode,
 )
import ScriptsV3 (
    alwaysTrueCodeV3,
    dutchDrepCredentialCode,
    dutchDrepLockScriptCode,
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
initTxOutRef :: PlutusV2.TxOutRef
initTxOutRef =
    PlutusV2.TxOutRef
        ((PlutusV2.TxId . P.integerToByteString BigEndian 32) 0x390c4a95769677080bc59280cd5ad28367d41d20b604f7d05a4cfb516e6a01a2)
        0

-- The above TxOutRef is applied to the V2 script to finalize the script
appliedDutchDrepNFTScriptCode :: CompiledCode (P.BuiltinData -> P.BuiltinData -> ())
appliedDutchDrepNFTScriptCode = dutchDrepNFTScriptCode `unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion100 (PlutusV3.toBuiltinData initTxOutRef)

scriptHashDutchDrepNFT :: ScriptHash
scriptHashDutchDrepNFT = hashScript . PlutusScript PlutusScriptV2 . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode $ appliedDutchDrepNFTScriptCode

-- The currency symbol of the DutchDrep NFT
dutchDrepNFTSymbol :: PlutusV3.CurrencySymbol
dutchDrepNFTSymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashDutchDrepNFT

-- The above currency symbol is applied to the script to finalize the script
appliedDutchDrepCredentialCode :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
appliedDutchDrepCredentialCode = dutchDrepCredentialCode `unsafeApplyCode` PlutusTx.liftCodeDef (PlutusV3.toBuiltinData dutchDrepNFTSymbol)

scriptHashDutchDrepCredential :: ScriptHash
scriptHashDutchDrepCredential = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ appliedDutchDrepCredentialCode

-- The script hash of the Dutch Drep lock script
dutchDrepLockScriptHash :: ScriptHash
dutchDrepLockScriptHash = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ dutchDrepLockScriptCode

main :: IO ()
main = do
    -- writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueV3.plutus" alwaysTrueCodeV3
    -- writeCodeToFile PlutusScriptV2 "./assets/V2/alwaysTrueV2.plutus" alwaysTrueCodeV2
    writeCodeToFile PlutusScriptV2 "./assets/V2/dutchDrepNFT.plutus" appliedDutchDrepNFTScriptCode
    print $ "Currency Symbol (PolicyID) of the Dutch Drep NFT: " ++ show dutchDrepNFTSymbol
    writeCodeToFile PlutusScriptV3 "./assets/V3/dutchDrepCredential.plutus" appliedDutchDrepCredentialCode
    print $ "Script Hash of Credential: " ++ show scriptHashDutchDrepCredential
    writeCodeToFile PlutusScriptV3 "./assets/V3/dutchDrepLockScript.plutus" dutchDrepLockScriptCode
    print $ "Script Hash of Lock Script: " ++ show dutchDrepLockScriptHash
