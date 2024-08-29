{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen.Spec
  ( Spec (..),
    MessageSpec (..),
    ComponentSpec (..),
    GroupSpec (..),
    MessagePiece (..),
    FieldSpec (..),
    FieldValueSpec (..),
    FieldType (..),
    fieldTypeIsData,
    parseSpec,
  )
where

import Control.Monad
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Text.Read
import Text.XML as XML

data Spec = Spec
  { specFields :: ![FieldSpec],
    specHeader :: ![MessagePiece],
    specTrailer :: ![MessagePiece],
    specComponents :: ![ComponentSpec],
    specMessages :: ![MessageSpec]
  }

parseSpec :: Document -> Maybe Spec
parseSpec doc = do
  let rootElements = subElements $ documentRoot doc

  fieldsElement <- find ((== "fields") . elementName) rootElements
  specFields <- mapM parseFieldSpec $ subElements fieldsElement

  headerElement <- find ((== "header") . elementName) rootElements
  specHeader <- mapM parseMessagePiece $ subElements headerElement

  trailerElement <- find ((== "trailer") . elementName) rootElements
  specTrailer <- mapM parseMessagePiece $ subElements trailerElement

  componentsElement <- find ((== "components") . elementName) rootElements
  specComponents <- mapM parseComponentSpec $ subElements componentsElement

  messagesElement <- find ((== "messages") . elementName) rootElements
  specMessages <- mapM parseMessageSpec $ subElements messagesElement

  pure Spec {..}

data MessageSpec = MessageSpec
  { messageName :: !Text,
    messageType :: !Text,
    messageCategory :: !Text,
    messagePieces :: ![MessagePiece]
  }
  deriving (Show)

parseMessageSpec :: Element -> Maybe MessageSpec
parseMessageSpec e@Element {..} = do
  guard $ elementName == "message"
  messageName <- M.lookup "name" elementAttributes
  messageType <- M.lookup "msgtype" elementAttributes
  messageCategory <- M.lookup "msgcat" elementAttributes
  messagePieces <- mapM parseMessagePiece $ subElements e

  pure MessageSpec {..}

data ComponentSpec = ComponentSpec
  { componentName :: !Text,
    componentPieces :: ![MessagePiece]
  }
  deriving (Show)

parseComponentSpec :: Element -> Maybe ComponentSpec
parseComponentSpec e@Element {..} = do
  guard $ elementName == "component"
  componentName <- M.lookup "name" elementAttributes
  componentPieces <- mapM parseMessagePiece $ subElements e

  pure ComponentSpec {..}

data MessagePiece
  = MessagePieceField !Text !Bool
  | MessagePieceComponent !Text !Bool
  | MessagePieceGroup !GroupSpec !Bool
  deriving (Show)

parseMessagePiece :: Element -> Maybe MessagePiece
parseMessagePiece e@Element {..} =
  case elementName of
    "field" -> do
      name <- M.lookup "name" elementAttributes
      required <- M.lookup "required" elementAttributes >>= parseRequired
      pure $ MessagePieceField name required
    "component" -> do
      name <- M.lookup "name" elementAttributes
      required <- M.lookup "required" elementAttributes >>= parseRequired
      pure $ MessagePieceComponent name required
    "group" -> do
      groupName <- M.lookup "name" elementAttributes
      required <- M.lookup "required" elementAttributes >>= parseRequired
      groupPieces <- mapM parseMessagePiece $ subElements e
      pure $ MessagePieceGroup GroupSpec {..} required
    _ -> Nothing

parseRequired :: Text -> Maybe Bool
parseRequired = \case
  "Y" -> pure True
  "N" -> pure False
  _ -> Nothing

data GroupSpec = GroupSpec
  { groupName :: !Text,
    groupPieces :: ![MessagePiece]
  }
  deriving (Show)

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

-- TODO Missing here: XML
fieldTypeIsData :: FieldType -> Bool
fieldTypeIsData = \case
  FieldTypeData -> True
  _ -> False

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
