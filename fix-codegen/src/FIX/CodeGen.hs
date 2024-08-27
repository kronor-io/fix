{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import Control.Monad
import Data.Char as Char
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FIX.CodeGen.OptParse
import FIX.CodeGen.Spec
import Language.Haskell.TH as TH
import Path
import Path.IO
import System.Exit
import System.Process
import qualified Text.XML as XML
import UnliftIO

runFixCodeGen :: IO ()
runFixCodeGen = do
  Settings {..} <- getSettings
  doc <- XML.readFile XML.def (fromAbsFile settingSpecFile)
  case parseSpec doc of
    Nothing -> die "Failed to parse specfication."
    Just spec' -> do
      let spec = filterSpec settingMessages spec'
      -- Fields
      let fieldSpecs = specFields spec
      writeFieldsFile settingOutputDir fieldSpecs
      writeFieldsGenFile settingOutputDir fieldSpecs
      writeFieldsSpecFile settingOutputDir fieldSpecs

      -- Messages
      writeMessagesClassFile settingOutputDir

      let messageSpecs = specMessages spec
      writeMessagesFile settingOutputDir messageSpecs
      writeMessagesGenFile settingOutputDir messageSpecs
      writeMessagesSpecFile settingOutputDir messageSpecs

filterSpec :: Maybe (Set Text) -> Spec -> Spec
filterSpec Nothing spec = spec
filterSpec (Just messages) spec =
  let filteredMessages = filter (\m -> S.member (messageName m) messages) (specMessages spec)
      pieceFieldNames :: MessagePiece -> Set Text
      pieceFieldNames = \case
        MessagePieceField n _ -> S.singleton n
        MessagePieceComponent c _ -> S.singleton c
        MessagePieceGroup g _ ps -> S.insert g (foldMap pieceFieldNames ps)
      messageFields :: MessageSpec -> Set Text
      messageFields = foldMap pieceFieldNames . messagePieces
      mentionedFields =
        S.unions
          [ foldMap pieceFieldNames (specHeader spec),
            foldMap messageFields filteredMessages,
            foldMap pieceFieldNames (specTrailer spec)
          ]
      filteredFields = filter (\f -> S.member (fieldName f) mentionedFields) (specFields spec)
   in spec
        { specMessages = filteredMessages,
          specFields = filteredFields
        }

fieldSpecConstructorName :: FieldSpec -> Name
fieldSpecConstructorName = mkName . T.unpack . fieldName

fieldValueSpecConstructorName :: FieldSpec -> FieldValueSpec -> Name
fieldValueSpecConstructorName FieldSpec {..} FieldValueSpec {..} =
  mkName $
    concat
      [ T.unpack fieldName,
        "_",
        T.unpack fieldValueDescription -- TODO fix the casing
      ]

writeFieldsFile :: Path Abs Dir -> [FieldSpec] -> IO ()
writeFieldsFile outputDir fieldSpecs = do
  fieldsDir <- resolveDir outputDir "fix-spec/src/FIX/Fields"

  forConcurrently_ fieldSpecs $ \f@FieldSpec {..} -> do
    fieldFile <- resolveFile fieldsDir $ T.unpack fieldName <> ".hs"
    let constructorName = fieldSpecConstructorName f
    let selectorName = mkName $ "un" <> T.unpack fieldName
    let typ = case fieldType of
          FieldTypeBoolean -> ConT (mkName "Bool")
          _ -> ConT (mkName "ByteString")
    let section =
          [ "-- | " <> show f,
            TH.pprint
              [ if null fieldValues
                  then
                    NewtypeD
                      []
                      constructorName
                      []
                      Nothing
                      (RecC constructorName [(selectorName, Bang NoSourceUnpackedness NoSourceStrictness, typ)])
                      [ DerivClause
                          (Just StockStrategy)
                          [ ConT (mkName "Show"),
                            ConT (mkName "Eq"),
                            ConT (mkName "Generic")
                          ]
                      ]
                  else
                    DataD
                      []
                      constructorName
                      []
                      Nothing
                      (map (\fvs -> NormalC (fieldValueSpecConstructorName f fvs) []) fieldValues)
                      [ DerivClause
                          (Just StockStrategy)
                          [ ConT (mkName "Show"),
                            ConT (mkName "Eq"),
                            ConT (mkName "Generic")
                          ]
                      ],
                InstanceD
                  Nothing
                  []
                  (AppT (ConT (mkName "Validity")) (ConT constructorName))
                  [],
                InstanceD
                  Nothing
                  []
                  (AppT (ConT (mkName "IsField")) (ConT constructorName))
                  [ FunD
                      (mkName "fieldTag")
                      [ Clause
                          [VarP (mkName "Proxy")]
                          (NormalB (LitE (IntegerL (toInteger fieldNumber))))
                          []
                      ],
                    FunD
                      (mkName "fieldIsData")
                      [ Clause
                          [VarP (mkName "Proxy")]
                          ( NormalB
                              ( ConE
                                  (mkName $ show $ fieldTypeIsData fieldType)
                              )
                          )
                          []
                      ],
                    FunD
                      (mkName "fieldToValue")
                      [ Clause
                          []
                          ( NormalB
                              ( if null fieldValues
                                  then
                                    InfixE
                                      (Just (VarE (mkName "toValue")))
                                      (VarE (mkName "."))
                                      (Just (VarE selectorName))
                                  else
                                    LamCaseE $
                                      map
                                        ( \fvs ->
                                            Match
                                              ( ConP
                                                  (fieldValueSpecConstructorName f fvs)
                                                  []
                                                  []
                                              )
                                              (NormalB (LitE (StringL (T.unpack (fieldValueEnum fvs)))))
                                              []
                                        )
                                        fieldValues
                              )
                          )
                          []
                      ],
                    FunD
                      (mkName "fieldFromValue")
                      [ Clause
                          []
                          ( NormalB
                              ( if null fieldValues
                                  then
                                    InfixE
                                      (Just (VarE (mkName "fromValue")))
                                      (VarE (mkName ">=>"))
                                      ( Just
                                          ( InfixE
                                              (Just (VarE (mkName "prettyValidate")))
                                              (VarE (mkName "."))
                                              (Just (VarE constructorName))
                                          )
                                      )
                                  else
                                    LamCaseE $
                                      concat
                                        [ map
                                            ( \fvs ->
                                                Match
                                                  (LitP (StringL (T.unpack (fieldValueEnum fvs))))
                                                  ( NormalB
                                                      ( AppE
                                                          (ConE (mkName "Right"))
                                                          (ConE (fieldValueSpecConstructorName f fvs))
                                                      )
                                                  )
                                                  []
                                            )
                                            fieldValues,
                                          [ let vn = mkName "v"
                                             in Match
                                                  (VarP vn)
                                                  ( NormalB
                                                      ( AppE
                                                          (ConE (mkName "Left"))
                                                          ( InfixE
                                                              (Just (LitE (StringL $ "Unknown " <> T.unpack fieldName <> ": ")))
                                                              (VarE (mkName "<>"))
                                                              (Just (AppE (VarE (mkName "show")) (VarE vn)))
                                                          )
                                                      )
                                                  )
                                                  []
                                          ]
                                        ]
                              )
                          )
                          []
                      ]
                  ]
              ]
          ]

    writeHaskellCode fieldFile $
      unlines $
        concat
          [ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}",
              "{-# LANGUAGE DerivingStrategies #-}",
              "{-# LANGUAGE DeriveGeneric #-}",
              "{-# LANGUAGE LambdaCase #-}",
              "{-# LANGUAGE OverloadedStrings #-}",
              "",
              "module FIX.Fields." <> T.unpack fieldName <> " where",
              "",
              "import Control.Monad",
              "import Data.ByteString (ByteString)",
              "import Data.Proxy",
              "import Data.Validity",
              "import FIX.Core (IsFieldType(..), IsField(..))",
              "import GHC.Generics (Generic)",
              "",
              "{-# ANN module (\"HLint: ignore\" :: String) #-}",
              ""
            ],
            section
          ]

writeFieldsGenFile :: Path Abs Dir -> [FieldSpec] -> IO ()
writeFieldsGenFile outputDir fieldSpecs = do
  fieldsGenFile <- resolveFile outputDir "fix-spec-gen/src/FIX/Fields/Gen.hs"
  let imports =
        map
          ( \f ->
              "import FIX.Fields." <> T.unpack (fieldName f)
          )
          fieldSpecs
  sections <- forM fieldSpecs $ \f -> do
    let constructorName = fieldSpecConstructorName f
    pure
      [ TH.pprint
          [ InstanceD Nothing [] (AppT (ConT (mkName "GenValid")) (ConT constructorName)) []
          ]
      ]
  writeHaskellCode fieldsGenFile $
    unlines $
      concat $
        [ "{-# OPTIONS_GHC -Wno-orphans #-}",
          "",
          "module FIX.Fields.Gen where",
          "",
          "import Data.GenValidity",
          "import Data.GenValidity.ByteString ()",
          ""
        ]
          : imports
          : sections

writeFieldsSpecFile :: Path Abs Dir -> [FieldSpec] -> IO ()
writeFieldsSpecFile outputDir fieldSpecs = do
  fieldsSpecFile <- resolveFile outputDir "fix-spec-gen/test/FIX/FieldsSpec.hs"
  let imports =
        map
          ( \f ->
              "import FIX.Fields." <> T.unpack (fieldName f)
          )
          fieldSpecs
  statements <- forM fieldSpecs $ \f -> do
    let constructorName = fieldSpecConstructorName f
    pure $ NoBindS (AppTypeE (VarE (mkName "fieldSpec")) (ConT constructorName))

  writeHaskellCode fieldsSpecFile $
    unlines $
      concat
        [ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
            "{-# LANGUAGE TypeApplications #-}",
            "",
            "module FIX.FieldsSpec where",
            "",
            "import FIX.Core.TestUtils",
            "import FIX.Fields.Gen ()",
            "import Test.Syd",
            ""
          ],
          imports,
          [ TH.pprint
              [ SigD (mkName "spec") (ConT (mkName "Spec")),
                FunD (mkName "spec") [Clause [] (NormalB (DoE Nothing statements)) []]
              ]
          ]
        ]

messageSpecConstructorName :: MessageSpec -> Name
messageSpecConstructorName = mkName . T.unpack . messageName

writeMessagesClassFile :: Path Abs Dir -> IO ()
writeMessagesClassFile outputDir = do
  messagesClassFile <- resolveFile outputDir "fix-spec/src/FIX/Messages/Class.hs"
  writeHaskellCode messagesClassFile $
    unlines
      [ "module FIX.Messages.Class where",
        ""
      ]

writeMessagesFile :: Path Abs Dir -> [MessageSpec] -> IO ()
writeMessagesFile outputDir messageSpecs = do
  messagesDir <- resolveDir outputDir "fix-spec/src/FIX/Messages"
  forConcurrently_ messageSpecs $ \f@MessageSpec {..} -> do
    messageFile <- resolveFile messagesDir $ T.unpack messageName <> ".hs"
    let constructorName = messageSpecConstructorName f
    -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards
    let recordWildCardName = mkName (T.unpack messageName <> "{..}")
    let imports =
          mapMaybe
            ( \case
                MessagePieceField t _ -> Just $ "import FIX.Fields." <> T.unpack t
                _ -> Nothing
            )
            messagePieces
        fieldName t = mkName $ lowerHead (T.unpack messageName) <> T.unpack t

    let section =
          [ "-- | " <> show f,
            TH.pprint
              [ DataD
                  []
                  constructorName
                  []
                  Nothing
                  [ RecC
                      constructorName
                      ( mapMaybe
                          ( \case
                              MessagePieceField t required ->
                                Just
                                  ( fieldName t,
                                    Bang NoSourceUnpackedness SourceStrict,
                                    ( if required
                                        then id
                                        else AppT (ConT (mkName "Maybe"))
                                    )
                                      $ ConT (mkName (T.unpack t))
                                  )
                              _ -> Nothing -- TODO define them all
                          )
                          messagePieces
                      )
                  ]
                  [ DerivClause
                      (Just StockStrategy)
                      [ ConT (mkName "Show"),
                        ConT (mkName "Eq"),
                        ConT (mkName "Generic")
                      ]
                  ],
                InstanceD
                  Nothing
                  []
                  (AppT (ConT (mkName "Validity")) (ConT constructorName))
                  [],
                InstanceD
                  Nothing
                  []
                  (AppT (AppT (ConT (mkName "IsMessage")) (ConT (mkName "MsgType"))) (ConT constructorName))
                  [ FunD
                      (mkName "messageType")
                      [ Clause
                          [VarP (mkName "Proxy")]
                          (NormalB (ConE (mkName ("MsgType_" <> T.unpack (T.toUpper messageName)))))
                          []
                      ],
                    FunD
                      (mkName "toMessageFields")
                      [ Clause
                          [ConP recordWildCardName [] []]
                          ( NormalB
                              ( AppE
                                  (VarE (mkName "catMaybes"))
                                  ( ListE
                                      ( mapMaybe
                                          ( \case
                                              MessagePieceField t required ->
                                                Just $
                                                  AppE
                                                    ( VarE
                                                        ( mkName
                                                            ( if required
                                                                then "requiredFieldB"
                                                                else "optionalFieldB"
                                                            )
                                                        )
                                                    )
                                                    (VarE (fieldName t))
                                              _ -> Nothing
                                          )
                                          messagePieces
                                      )
                                  )
                              )
                          )
                          []
                      ],
                    FunD
                      (mkName "fromMessageFields")
                      [ Clause
                          -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards
                          []
                          ( NormalB
                              ( DoE
                                  Nothing
                                  $ concat
                                    [ mapMaybe
                                        ( \case
                                            MessagePieceField t required ->
                                              Just $
                                                BindS
                                                  (VarP (fieldName t))
                                                  ( VarE
                                                      ( mkName
                                                          ( if required
                                                              then "requiredFieldP"
                                                              else "optionalFieldP"
                                                          )
                                                      )
                                                  )
                                            _ -> Nothing
                                        )
                                        messagePieces,
                                      [NoBindS $ AppE (VarE (mkName "pure")) (ConE recordWildCardName)]
                                    ]
                              )
                          )
                          []
                      ]
                  ]
              ]
          ]

    writeHaskellCode messageFile $
      unlines $
        concat
          [ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}",
              "{-# LANGUAGE DeriveGeneric #-}",
              "{-# LANGUAGE MultiParamTypeClasses #-}",
              "{-# LANGUAGE DerivingStrategies #-}",
              "{-# LANGUAGE RecordWildCards #-}",
              "",
              "module FIX.Messages." <> T.unpack messageName <> " where",
              "",
              "import Data.Validity",
              "import FIX.Core (IsMessage(..), requiredFieldB, optionalFieldB, requiredFieldP, optionalFieldP)",
              "import FIX.Fields.MsgType",
              "import GHC.Generics (Generic)",
              "import Data.Proxy",
              "import Data.Maybe (catMaybes)",
              ""
            ],
            imports,
            section
          ]

writeMessagesGenFile :: Path Abs Dir -> [MessageSpec] -> IO ()
writeMessagesGenFile outputDir messageSpecs = do
  messagesGenFile <- resolveFile outputDir "fix-spec-gen/src/FIX/Messages/Gen.hs"
  let messagesImports =
        map
          ( \f ->
              "import FIX.Messages." <> T.unpack (messageName f)
          )
          messageSpecs
  sections <- forM messageSpecs $ \f -> do
    let constructorName = messageSpecConstructorName f
    pure
      [ TH.pprint
          [ InstanceD Nothing [] (AppT (ConT (mkName "GenValid")) (ConT constructorName)) []
          ]
      ]
  writeHaskellCode messagesGenFile $
    unlines $
      concat $
        [ "{-# OPTIONS_GHC -Wno-orphans #-}",
          "",
          "module FIX.Messages.Gen where",
          "",
          "import Data.GenValidity",
          "import Data.GenValidity.ByteString ()",
          "import FIX.Fields.Gen ()",
          ""
        ]
          : messagesImports
          : sections

writeMessagesSpecFile :: Path Abs Dir -> [MessageSpec] -> IO ()
writeMessagesSpecFile outputDir messageSpecs = do
  messagesSpecFile <- resolveFile outputDir "fix-spec-gen/test/FIX/MessagesSpec.hs"
  let imports =
        map
          ( \f ->
              "import FIX.Messages." <> T.unpack (messageName f)
          )
          messageSpecs
  statements <- forM messageSpecs $ \f -> do
    let constructorName = messageSpecConstructorName f
    pure $ NoBindS (AppTypeE (VarE (mkName "messageSpec")) (ConT constructorName))

  writeHaskellCode messagesSpecFile $
    unlines $
      concat
        [ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
            "{-# LANGUAGE TypeApplications #-}",
            "",
            "module FIX.MessagesSpec where",
            "",
            "import FIX.Core.TestUtils",
            "import FIX.Messages.Gen ()",
            "import Test.Syd",
            ""
          ],
          imports,
          [ TH.pprint
              [ SigD (mkName "spec") (ConT (mkName "Spec")),
                FunD (mkName "spec") [Clause [] (NormalB (DoE Nothing statements)) []]
              ]
          ]
        ]

writeHaskellCode :: Path Abs File -> String -> IO ()
writeHaskellCode f source = do
  ensureDir (parent f)
  putStrLn $ unwords ["Writing", fromAbsFile f]
  writeFile (fromAbsFile f) $ disclaimer <> source
  runOrmoluOn f

disclaimer :: String
disclaimer =
  unlines
    [ "-- | This module has been GENERATED by fix-codegen.",
      "-- Any manual edits will be undone the next time fix-codegen is run."
    ]

runOrmoluOn :: Path Abs File -> IO ()
runOrmoluOn f = callProcess "ormolu" ["-i", "-c", fromAbsFile f]

lowerHead :: String -> String
lowerHead = \case
  [] -> []
  (c : cs) -> Char.toLower c : cs
