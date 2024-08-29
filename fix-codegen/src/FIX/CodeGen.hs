{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Path.IO
import Paths_fix_codegen (getDataDir)
import System.Exit
import Text.Show.Pretty (ppShow)
import qualified Text.XML as XML

runFixCodeGen :: IO ()
runFixCodeGen = do
  Settings {..} <- getSettings
  doc <- XML.readFile XML.def (fromAbsFile settingSpecFile)
  case parseSpec doc of
    Nothing -> die "Failed to parse specfication."
    Just spec' -> do
      let spec = filterSpec settingMessages spec'

      testResourcesFiles <- genTestResources

      runCodeGen settingOutputDir $
        mconcat
          [ genDataFile "fix-spec/package.yaml",
            genDataFile "fix-spec/fix-spec.cabal",
            genDataFile "fix-spec/default.nix",
            genDataFile "fix-spec-gen/package.yaml",
            genDataFile "fix-spec-gen/fix-spec-gen.cabal",
            genDataFile "fix-spec-gen/default.nix",
            let fieldSpecs = specFields spec
             in mconcat
                  [ fieldsDataFiles fieldSpecs,
                    fieldsGenFile fieldSpecs,
                    fieldsSpecFile fieldSpecs,
                    topLevelFieldsFile fieldSpecs
                  ],
            headerDataFile (specHeader spec),
            trailerDataFile (specTrailer spec),
            -- Groups
            let groupSpecs = gatherGroupSpecs spec
             in mconcat
                  [ genHaskellDataFile "fix-spec/src/FIX/Groups/Class.hs",
                    groupsDataFiles groupSpecs
                    -- componentsGenFile componentSpecs,
                    -- genHaskellDataFile "fix-spec-gen/src/FIX/Components/TestUtils.hs",
                    -- componentsSpecFile componentSpecs
                  ],
            -- Components
            let componentSpecs = specComponents spec
             in mconcat
                  [ genHaskellDataFile "fix-spec/src/FIX/Components/Class.hs",
                    componentsDataFiles componentSpecs
                    -- componentsGenFile componentSpecs,
                    -- genHaskellDataFile "fix-spec-gen/src/FIX/Components/TestUtils.hs",
                    -- componentsSpecFile componentSpecs
                  ],
            -- Messages
            let messageSpecs = specMessages spec
             in mconcat
                  [ genHaskellDataFile "fix-spec/src/FIX/Messages/Class.hs",
                    messagesDataFiles messageSpecs,
                    genHaskellDataFile "fix-spec/src/FIX/Messages/Envelope.hs",
                    messagesGenFile messageSpecs,
                    genHaskellDataFile "fix-spec-gen/src/FIX/Messages/TestUtils.hs",
                    messagesSpecFile messageSpecs
                  ],
            genHaskellDataFile "fix-spec-gen/test/Spec.hs",
            testResourcesFiles
          ]

filterSpec :: Maybe (Set Text) -> Spec -> Spec
filterSpec Nothing spec = spec
filterSpec (Just messages) spec =
  let filteredMessages = filter (\m -> S.member (messageName m) messages) (specMessages spec)
      pieceComponentNames :: MessagePiece -> Set Text
      pieceComponentNames = \case
        MessagePieceField _ _ -> S.empty
        MessagePieceComponent c _ -> S.singleton c
        MessagePieceGroup ps _ -> foldMap pieceComponentNames (groupPieces ps)
      messageComponents :: MessageSpec -> Set Text
      messageComponents = foldMap pieceComponentNames . messagePieces
      mentionedComponents :: Set Text
      mentionedComponents = foldMap messageComponents filteredMessages
      filteredCompoments = filter (\f -> S.member (componentName f) mentionedComponents) (specComponents spec)
      pieceFieldNames :: MessagePiece -> Set Text
      pieceFieldNames = \case
        MessagePieceField n _ -> S.singleton n
        MessagePieceComponent _ _ -> S.empty
        MessagePieceGroup gs _ -> S.insert (groupName gs) (foldMap pieceFieldNames (groupPieces gs))
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
          specComponents = filteredCompoments,
          specFields = filteredFields
        }

pieceGroupSpecs :: MessagePiece -> Set GroupSpec
pieceGroupSpecs = \case
  MessagePieceGroup gs _ -> S.insert gs (foldMap pieceGroupSpecs (groupPieces gs))
  MessagePieceField _ _ -> S.empty
  MessagePieceComponent _ _ -> S.empty

fieldSpecConstructorName :: FieldSpec -> Name
fieldSpecConstructorName = mkName . T.unpack . fieldName

fieldValueSpecConstructorName :: FieldSpec -> FieldValueSpec -> Name
fieldValueSpecConstructorName FieldSpec {..} FieldValueSpec {..} =
  mkName $
    concat
      [ upperHead (T.unpack fieldName),
        toPascalCase (T.unpack fieldValueDescription)
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
          [ commentString $ ppShow f,
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
                validityInstance constructorName,
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

mkFieldName :: Text -> Text -> Name
mkFieldName name t = mkName $ lowerHead (T.unpack name) <> T.unpack t

messagePiecesDataDeclaration :: Text -> [MessagePiece] -> Dec
messagePiecesDataDeclaration name pieces =
  let constructorName = mkName (T.unpack name)
      requiredFunc r =
        if r
          then id
          else AppT (ConT (mkName "Maybe"))
   in DataD
        []
        constructorName
        []
        Nothing
        [ RecC
            constructorName
            ( map
                ( \case
                    MessagePieceField t required ->
                      ( mkFieldName name t,
                        Bang NoSourceUnpackedness SourceStrict,
                        requiredFunc required $
                          ConT (mkName (T.unpack t))
                      )
                    MessagePieceComponent c required ->
                      ( mkFieldName name c,
                        Bang NoSourceUnpackedness SourceStrict,
                        requiredFunc required $
                          ConT (mkName (T.unpack c))
                      )
                    MessagePieceGroup gs required ->
                      ( mkFieldName name (groupSpecConstructorName gs),
                        Bang NoSourceUnpackedness SourceStrict,
                        requiredFunc required $
                          AppT (ConT (mkName "NonEmpty")) (ConT (mkName (T.unpack (groupSpecConstructorName gs))))
                      )
                )
                pieces
            )
        ]
        [ DerivClause
            (Just StockStrategy)
            [ ConT (mkName "Show"),
              ConT (mkName "Eq"),
              ConT (mkName "Generic")
            ]
        ]

validityInstance :: Name -> Dec
validityInstance typeName =
  InstanceD
    Nothing
    []
    (AppT (ConT (mkName "Validity")) (ConT typeName))
    []

messagePiecesImports :: [MessagePiece] -> [String]
messagePiecesImports =
  map
    ( \case
        MessagePieceField t _ -> "import FIX.Fields." <> T.unpack t
        MessagePieceComponent t _ -> "import FIX.Components." <> T.unpack t
        MessagePieceGroup gs _ -> "import FIX.Groups." <> T.unpack (groupSpecConstructorName gs)
    )

messagePiecesToFieldsFunction :: Text -> Name -> [MessagePiece] -> Dec
messagePiecesToFieldsFunction name funName pieces =
  let builders =
        map
          ( \case
              MessagePieceField t required ->
                AppE
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredFieldB"
                              else "optionalFieldB"
                          )
                      )
                  )
                  (VarE (mkFieldName name t))
              MessagePieceComponent c required ->
                AppE
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredComponentB"
                              else "optionalComponentB"
                          )
                      )
                  )
                  (VarE (mkFieldName name c))
              MessagePieceGroup gs required ->
                AppE
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredGroupB"
                              else "optionalGroupB"
                          )
                      )
                  )
                  (VarE (mkFieldName name (groupSpecConstructorName gs)))
          )
          pieces
      recordWildCardName =
        mkName $
          -- We can get rid of this 'if' once all pieces have a definition
          (if null builders then id else (<> "{..}"))
            (T.unpack name)
   in FunD
        funName
        [ Clause
            [ConP recordWildCardName [] []]
            ( NormalB
                ( AppE
                    (VarE (mkName "concat"))
                    (ListE builders)
                )
            )
            []
        ]

messagePiecesFromFieldsFunction :: Text -> Name -> [MessagePiece] -> Dec
messagePiecesFromFieldsFunction name funName pieces =
  let statements =
        map
          ( \case
              MessagePieceField t required ->
                BindS
                  (VarP (mkFieldName name t))
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredFieldP"
                              else "optionalFieldP"
                          )
                      )
                  )
              MessagePieceComponent c required ->
                BindS
                  (VarP (mkFieldName name c))
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredComponentP"
                              else "optionalComponentP"
                          )
                      )
                  )
              MessagePieceGroup gs required ->
                BindS
                  (VarP (mkFieldName name (groupSpecConstructorName gs)))
                  ( VarE
                      ( mkName
                          ( if required
                              then "requiredGroupP"
                              else "optionalGroupP"
                          )
                      )
                  )
          )
          pieces
      recordWildCardName =
        mkName $
          -- We can get rid of this 'if' once all pieces have a definition
          (if null statements then id else (<> "{..}"))
            (T.unpack name)
   in FunD
        funName
        [ Clause
            []
            ( NormalB
                ( DoE
                    Nothing
                    $ concat
                      [ statements,
                        [NoBindS $ AppE (VarE (mkName "pure")) (ConE recordWildCardName)]
                      ]
                )
            )
            []
        ]

messagePiecesIsComponentInstance :: Text -> [MessagePiece] -> Dec
messagePiecesIsComponentInstance name pieces =
  InstanceD
    Nothing
    []
    (AppT (ConT (mkName "IsComponent")) (ConT (mkName (T.unpack name))))
    [ messagePiecesToFieldsFunction name (mkName "toComponentFields") pieces,
      messagePiecesFromFieldsFunction name (mkName "fromComponentFields") pieces
    ]

headerDataFile :: [MessagePiece] -> CodeGen
headerDataFile pieces =
  genHaskellFile "fix-spec/src/FIX/Messages/Header.hs" $
    let imports = messagePiecesImports pieces
     in unlines $
          concat
            [ [ "{-# LANGUAGE DerivingStrategies #-}",
                "{-# LANGUAGE DeriveGeneric #-}",
                "{-# LANGUAGE RecordWildCards #-}",
                "",
                "module FIX.Messages.Header where",
                "",
                "import Data.Maybe",
                "import Data.List.NonEmpty (NonEmpty)",
                "import Data.Validity",
                "import GHC.Generics (Generic)",
                "import FIX.Components.Class",
                "import FIX.Groups.Class",
                ""
              ],
              imports,
              [ TH.pprint $ messagePiecesDataDeclaration "Header" pieces,
                TH.pprint $ validityInstance (mkName "Header"),
                TH.pprint $ messagePiecesIsComponentInstance "Header" pieces
              ]
            ]

trailerDataFile :: [MessagePiece] -> CodeGen
trailerDataFile pieces =
  genHaskellFile "fix-spec/src/FIX/Messages/Trailer.hs" $
    let imports = messagePiecesImports pieces
     in unlines $
          concat
            [ [ "{-# LANGUAGE DerivingStrategies #-}",
                "{-# LANGUAGE DeriveGeneric #-}",
                "{-# LANGUAGE RecordWildCards #-}",
                "",
                "module FIX.Messages.Trailer where",
                "",
                "import Data.Validity",
                "import GHC.Generics (Generic)",
                "import FIX.Components.Class",
                ""
              ],
              imports,
              [ TH.pprint $ messagePiecesDataDeclaration "Trailer" pieces,
                TH.pprint $ validityInstance (mkName "Trailer"),
                TH.pprint $ messagePiecesIsComponentInstance "Trailer" pieces
              ]
            ]

gatherGroupSpecs :: Spec -> [GroupSpec]
gatherGroupSpecs Spec {..} =
  S.toList $
    mconcat
      [ foldMap pieceGroupSpecs specHeader,
        foldMap pieceGroupSpecs specTrailer,
        foldMap pieceGroupSpecs $ foldMap componentPieces specComponents,
        foldMap pieceGroupSpecs $ foldMap messagePieces specMessages
      ]

groupSpecConstructorName :: GroupSpec -> Text
groupSpecConstructorName = (\t -> fromMaybe t $ T.stripPrefix "No" t) . groupName

groupsDataFiles :: [GroupSpec] -> CodeGen
groupsDataFiles = foldMap $ \f@GroupSpec {..} ->
  let constructorName = groupSpecConstructorName f
   in genHaskellFile ("fix-spec/src/FIX/Groups/" <> T.unpack constructorName <> ".hs") $
        -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards

        let section =
              [ commentString $ ppShow f,
                TH.pprint
                  [ messagePiecesDataDeclaration constructorName groupPieces,
                    validityInstance (mkName (T.unpack constructorName)),
                    messagePiecesIsComponentInstance constructorName groupPieces
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
                    "module FIX.Groups." <> T.unpack constructorName <> " where",
                    "",
                    "import Data.Validity",
                    "import FIX.Groups.Class",
                    "import FIX.Components.Class",
                    "import FIX.Fields.MsgType",
                    "import GHC.Generics (Generic)",
                    "import Data.Proxy",
                    ""
                  ],
                  messagePiecesImports groupPieces,
                  section
                ]

componentSpecConstructorName :: ComponentSpec -> Name
componentSpecConstructorName = mkName . T.unpack . componentName

componentsDataFiles :: [ComponentSpec] -> CodeGen
componentsDataFiles = foldMap $ \f@ComponentSpec {..} ->
  genHaskellFile ("fix-spec/src/FIX/Components/" <> T.unpack componentName <> ".hs") $
    let constructorName = componentSpecConstructorName f
        -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards

        section =
          [ commentString $ ppShow f,
            TH.pprint
              [ messagePiecesDataDeclaration componentName componentPieces,
                validityInstance constructorName,
                messagePiecesIsComponentInstance componentName componentPieces
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
                "module FIX.Components." <> T.unpack componentName <> " where",
                "",
                "import Data.Validity",
                "import FIX.Components.Class",
                "import FIX.Components.Class",
                "import FIX.Fields.MsgType",
                "import GHC.Generics (Generic)",
                "import Data.Proxy",
                ""
              ],
              messagePiecesImports componentPieces,
              section
            ]

messagesDataFiles :: [MessageSpec] -> CodeGen
messagesDataFiles = foldMap $ \f@MessageSpec {..} ->
  genHaskellFile ("fix-spec/src/FIX/Messages/" <> T.unpack messageName <> ".hs") $
    let constructorName = messageSpecConstructorName f
        -- This is an ugly hack because Language.Haskell.TH.Syntax does not have any syntax for record wildcards

        section =
          [ commentString $ ppShow f,
            TH.pprint
              [ messagePiecesDataDeclaration messageName messagePieces,
                validityInstance constructorName,
                messagePiecesIsComponentInstance messageName messagePieces,
                InstanceD
                  Nothing
                  []
                  (AppT (ConT (mkName "IsMessage")) (ConT constructorName))
                  [ FunD
                      (mkName "messageType")
                      [ Clause
                          [VarP (mkName "Proxy")]
                          (NormalB (ConE (mkName ("MsgType" <> upperHead (T.unpack messageName)))))
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
                "import Data.List.NonEmpty (NonEmpty)",
                "import FIX.Components.Class",
                "import FIX.Groups.Class",
                "import FIX.Messages.Class",
                "import FIX.Fields.MsgType",
                "import GHC.Generics (Generic)",
                "import Data.Proxy",
                ""
              ],
              messagePiecesImports messagePieces,
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
              "import FIX.Messages.Header",
              "import FIX.Messages.Trailer",
              "import FIX.Messages.Envelope",
              ""
            ]
              : messagesImports
              : [ "",
                  "instance GenValid Header",
                  "instance GenValid Trailer",
                  "instance (GenValid a) => GenValid (Envelope a)",
                  ""
                ]
              : sections

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
           in NoBindS
                ( AppE
                    (AppTypeE (VarE (mkName "messageSpec")) (ConT constructorName))
                    (LitE (StringL (T.unpack (messageName f))))
                )
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

genTestResources :: IO CodeGen
genTestResources = do
  dataDir <- getDataDir >>= resolveDir' >>= (`resolveDir` "data")
  let subdir = [reldir|fix-spec-gen/test_resources|]
  files <- fmap (fromMaybe []) $ forgivingAbsence $ snd <$> listDirRecurRel (dataDir </> subdir)
  pure $ foldMap (genDataFile . fromRelFile . (subdir </>)) files

commentString :: String -> String
commentString s =
  let ls = lines s
   in case ls of
        [] -> []
        (h : rest) -> unlines $ ("-- | " <> h) : map ("-- " <>) rest
