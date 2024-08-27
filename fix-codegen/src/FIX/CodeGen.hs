{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FIX.CodeGen.Code
import FIX.CodeGen.OptParse
import FIX.CodeGen.Spec
import Language.Haskell.TH as TH
import Path
import System.Exit
import qualified Text.XML as XML

runFixCodeGen :: IO ()
runFixCodeGen = do
  Settings {..} <- getSettings
  doc <- XML.readFile XML.def (fromAbsFile settingSpecFile)
  case parseSpec doc of
    Nothing -> die "Failed to parse specfication."
    Just spec' -> do
      let spec = filterSpec settingMessages spec'

      runCodeGen settingOutputDir $
        mconcat
          [ let fieldSpecs = specFields spec
             in mconcat
                  [ fieldsDataFiles fieldSpecs,
                    fieldsGenFile fieldSpecs,
                    fieldsSpecFile fieldSpecs,
                    topLevelFieldsFile fieldSpecs
                  ],
            -- Messages
            let messageSpecs = specMessages spec
             in mconcat
                  [ messagesClassFile,
                    messagesDataFiles messageSpecs,
                    messagesGenFile messageSpecs,
                    messagesTestUtilsFile,
                    messagesSpecFile messageSpecs
                  ]
          ]

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

fieldsDataFiles :: [FieldSpec] -> CodeGen
fieldsDataFiles = foldMap $ \f@FieldSpec {..} ->
  genHaskellFile ("fix-spec/src/FIX/Fields/" <> T.unpack fieldName <> ".hs") $
    let constructorName = fieldSpecConstructorName f
        selectorName = mkName $ "un" <> T.unpack fieldName
        typ = case fieldType of
          FieldTypeBoolean -> ConT (mkName "Bool")
          FieldTypeLength -> ConT (mkName "Word")
          FieldTypeSeqNum -> ConT (mkName "Word")
          FieldTypeNumInGroup -> ConT (mkName "Word")
          FieldTypeInt -> ConT (mkName "Int")
          FieldTypeData -> ConT (mkName "DataBytes")
          FieldTypeString -> ConT (mkName "SimpleBytes")
          FieldTypeUTCTimestamp -> ConT (mkName "UTCTimestamp")
          _ -> ConT (mkName "SimpleBytes")
        section =
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
     in unlines $
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
                "import FIX.Core",
                "import GHC.Generics (Generic)",
                "",
                "{-# ANN module (\"HLint: ignore\" :: String) #-}",
                ""
              ],
              section
            ]

fieldsGenFile :: [FieldSpec] -> CodeGen
fieldsGenFile fieldSpecs =
  genHaskellFile "fix-spec-gen/src/FIX/Fields/Gen.hs" $
    let imports =
          map
            ( \f ->
                "import FIX.Fields." <> T.unpack (fieldName f)
            )
            fieldSpecs
        sections = flip map fieldSpecs $ \f ->
          let constructorName = fieldSpecConstructorName f
           in [ TH.pprint
                  [ InstanceD Nothing [] (AppT (ConT (mkName "GenValid")) (ConT constructorName)) []
                  ]
              ]
     in unlines $
          concat $
            [ "{-# OPTIONS_GHC -Wno-orphans #-}",
              "",
              "module FIX.Fields.Gen where",
              "",
              "import Data.GenValidity",
              "import Data.GenValidity.ByteString ()",
              "import FIX.Core.Gen ()",
              ""
            ]
              : imports
              : sections

fieldsSpecFile :: [FieldSpec] -> CodeGen
fieldsSpecFile fieldSpecs =
  genHaskellFile "fix-spec-gen/test/FIX/FieldsSpec.hs" $
    let imports =
          map
            ( \f ->
                "import FIX.Fields." <> T.unpack (fieldName f)
            )
            fieldSpecs
        statements = flip map fieldSpecs $ \f ->
          let constructorName = fieldSpecConstructorName f
           in NoBindS (AppTypeE (VarE (mkName "fieldSpec")) (ConT constructorName))
     in unlines $
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

topLevelFieldsFile :: [FieldSpec] -> CodeGen
topLevelFieldsFile fieldSpecs =
  genHaskellFile "fix-spec/src/FIX/Fields.hs" $
    let imports =
          map
            ( \f ->
                "import FIX.Fields." <> T.unpack (fieldName f) <> " as X"
            )
            fieldSpecs
     in unlines $
          concat
            [ [ "module FIX.Fields (module X) where",
                ""
              ],
              imports
            ]

messageSpecConstructorName :: MessageSpec -> Name
messageSpecConstructorName = mkName . T.unpack . messageName

messagesClassFile :: CodeGen
messagesClassFile = genDataFile "fix-spec/src/FIX/Messages/Class.hs"

messagesDataFiles :: [MessageSpec] -> CodeGen
messagesDataFiles = foldMap $ \f@MessageSpec {..} ->
  genHaskellFile ("fix-spec/src/FIX/Messages/" <> T.unpack messageName <> ".hs") $
    let constructorName = messageSpecConstructorName f
        -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards
        recordWildCardName = mkName (T.unpack messageName <> "{..}")
        imports =
          mapMaybe
            ( \case
                MessagePieceField t _ -> Just $ "import FIX.Fields." <> T.unpack t
                _ -> Nothing
            )
            messagePieces
        fieldName t = mkName $ lowerHead (T.unpack messageName) <> T.unpack t

        section =
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
                  (AppT (ConT (mkName "IsMessage")) (ConT constructorName))
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
     in unlines $
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
                "import FIX.Messages.Class",
                "import FIX.Fields.MsgType",
                "import GHC.Generics (Generic)",
                "import Data.Proxy",
                "import Data.Maybe (catMaybes)",
                ""
              ],
              imports,
              section
            ]

messagesGenFile :: [MessageSpec] -> CodeGen
messagesGenFile messageSpecs =
  genHaskellFile "fix-spec-gen/src/FIX/Messages/Gen.hs" $
    let messagesImports =
          map
            ( \f ->
                "import FIX.Messages." <> T.unpack (messageName f)
            )
            messageSpecs
        sections = flip map messageSpecs $ \f ->
          let constructorName = messageSpecConstructorName f
           in [ TH.pprint
                  [ InstanceD Nothing [] (AppT (ConT (mkName "GenValid")) (ConT constructorName)) []
                  ]
              ]
     in unlines $
          concat $
            [ "{-# OPTIONS_GHC -Wno-orphans #-}",
              "",
              "module FIX.Messages.Gen where",
              "",
              "import Data.GenValidity",
              "import Data.GenValidity.ByteString ()",
              "import FIX.Fields.Gen ()",
              "import FIX.Messages.Class",
              ""
            ]
              : messagesImports
              : [ "",
                  "instance GenValid MessageHeader",
                  "instance GenValid MessageTrailer",
                  "instance (GenValid a) => GenValid (Envelope a)",
                  ""
                ]
              : sections

messagesTestUtilsFile :: CodeGen
messagesTestUtilsFile = genDataFile "fix-spec-gen/src/FIX/Messages/TestUtils.hs"

messagesSpecFile :: [MessageSpec] -> CodeGen
messagesSpecFile messageSpecs =
  genHaskellFile "fix-spec-gen/test/FIX/MessagesSpec.hs" $
    let imports =
          map
            ( \f ->
                "import FIX.Messages." <> T.unpack (messageName f)
            )
            messageSpecs
        statements = flip map messageSpecs $ \f ->
          let constructorName = messageSpecConstructorName f
           in NoBindS (AppTypeE (VarE (mkName "messageSpec")) (ConT constructorName))
     in unlines $
          concat
            [ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
                "{-# LANGUAGE TypeApplications #-}",
                "",
                "module FIX.MessagesSpec where",
                "",
                "import FIX.Messages.TestUtils",
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
