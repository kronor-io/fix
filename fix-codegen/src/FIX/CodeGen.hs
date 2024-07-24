{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import Control.Monad
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import OptEnvConf
import Path
import Path.IO
import Paths_fix_codegen (version)
import System.Exit
import Text.Read
import Text.XML as XML

runFixCodeGen :: IO ()
runFixCodeGen = do
  settings <- getSettings
  doc <- XML.readFile def (fromAbsFile (settingsSpecFile settings))
  case parseSpec doc of
    Nothing -> die "Failed to parse specfication."
    Just spec -> do
      mapM_ print $ specFields spec

getSettings :: IO Settings
getSettings = runSettingsParser version "FIX protocol code generation"

data Settings = Settings {settingsSpecFile :: Path Abs File}

instance HasParser Settings where
  settingsParser = do
    settingsSpecFile <-
      mapIO (`resolveFile` "FIX44.xml") $
        directoryPathSetting
          [ help "path to directory with specificaton files",
            env "FIX_SPEC_DIR"
          ]
    pure Settings {..}

data Spec = Spec
  { specFields :: [FieldSpec]
  }

parseSpec :: Document -> Maybe Spec
parseSpec doc = do
  let rootElements = subElements $ documentRoot doc
  fieldsElement <- find ((== "fields") . elementName) rootElements
  specFields <- mapM parseFieldSpec $ subElements fieldsElement
  pure Spec {..}

data FieldSpec = FieldSpec
  { fieldNumber :: !Word,
    fieldName :: !Text,
    fieldType :: !FieldType,
    fieldValues :: ![FieldValueSpec]
  }
  deriving (Show)

parseFieldSpec :: Element -> Maybe FieldSpec
parseFieldSpec e@Element {..} = do
  guard $ elementName == "field"
  fieldNumber <- M.lookup "number" elementAttributes >>= readMaybe . T.unpack
  fieldName <- M.lookup "name" elementAttributes
  fieldType <- M.lookup "type" elementAttributes >>= parseFieldType
  fieldValues <- mapM parseFieldValueSpec $ subElements e
  pure FieldSpec {..}

data FieldType
  = FieldTypeBoolean
  | FieldTypeString
  | FieldTypeChar
  | FieldTypeInt
  | FieldTypePrice
  | FieldTypeSeqNum
  | FieldTypeLength
  | FieldTypeAMT
  | FieldTypeQTY
  | FieldTypeCurrency
  | FieldTypeMultipleValueString
  | FieldTypeExchange
  | FieldTypeNumInGroup
  | FieldTypeUTCTimestamp
  | FieldTypeLocalMktDate
  | FieldTypeData
  | FieldTypeFloat
  | FieldTypePercentage
  | FieldTypePriceOffset
  | FieldTypeMonthYear
  | FieldTypeUTCDateOnly
  | FieldTypeUTCTimeOnly
  | FieldTypeCountry
  deriving (Show)

parseFieldType :: Text -> Maybe FieldType
parseFieldType = \case
  "BOOLEAN" -> Just FieldTypeBoolean
  "STRING" -> Just FieldTypeString
  "CHAR" -> Just FieldTypeChar
  "INT" -> Just FieldTypeInt
  "PRICE" -> Just FieldTypePrice
  "SEQNUM" -> Just FieldTypeSeqNum
  "LENGTH" -> Just FieldTypeLength
  "AMT" -> Just FieldTypeAMT
  "QTY" -> Just FieldTypeQTY
  "CURRENCY" -> Just FieldTypeCurrency
  "MULTIPLEVALUESTRING" -> Just FieldTypeMultipleValueString
  "EXCHANGE" -> Just FieldTypeExchange
  "NUMINGROUP" -> Just FieldTypeNumInGroup
  "UTCTIMESTAMP" -> Just FieldTypeUTCTimestamp
  "LOCALMKTDATE" -> Just FieldTypeLocalMktDate
  "DATA" -> Just FieldTypeData
  "FLOAT" -> Just FieldTypeFloat
  "PERCENTAGE" -> Just FieldTypePercentage
  "PRICEOFFSET" -> Just FieldTypePriceOffset
  "MONTHYEAR" -> Just FieldTypeMonthYear
  "UTCDATEONLY" -> Just FieldTypeUTCDateOnly
  "UTCTIMEONLY" -> Just FieldTypeUTCTimeOnly
  "COUNTRY" -> Just FieldTypeCountry
  t -> traceShow t Nothing

data FieldValueSpec = FieldValueSpec
  { fieldValueEnum :: !Text,
    fieldValueDescription :: !Text
  }
  deriving (Show)

parseFieldValueSpec :: Element -> Maybe FieldValueSpec
parseFieldValueSpec Element {..} = do
  guard $ elementName == "value"
  fieldValueEnum <- M.lookup "enum" elementAttributes
  fieldValueDescription <- M.lookup "description" elementAttributes
  pure FieldValueSpec {..}

subElements :: Element -> [Element]
subElements =
  mapMaybe
    ( \case
        NodeElement e -> Just e
        _ -> Nothing
    )
    . elementNodes
