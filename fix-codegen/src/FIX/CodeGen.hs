{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import Control.Monad
import Data.List (find)
import qualified Data.Map as M
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
      let spec =
            fixupNestedOptionals $
              makeFirstGroupElementsRequired $
                simplifyGroupComponents $
                  foldDataFields $
                    filterSpec settingMessages spec'
      let groupSpecs = gatherGroupSpecs spec

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
                    topLevelFieldsFile fieldSpecs,
                    genHaskellDataFile "fix-spec-gen/test/FIX/AnyFieldSpec.hs"
                  ],
            headerDataFile (specHeader spec),
            trailerDataFile (specTrailer spec),
            -- Groups
            mconcat
              [ genHaskellDataFile "fix-spec/src/FIX/Groups/Class.hs",
                groupsDataFiles groupSpecs,
                -- groupsGenFile groupSpecs,
                genHaskellDataFile "fix-spec-gen/src/FIX/Groups/TestUtils.hs"
                -- groupsSpecFile groupSpecs
              ],
            -- Components
            let componentSpecs = specComponents spec
             in mconcat
                  [ genHaskellDataFile "fix-spec/src/FIX/Components/Class.hs",
                    componentsDataFiles componentSpecs,
                    componentsGenFile groupSpecs componentSpecs,
                    genHaskellDataFile "fix-spec-gen/src/FIX/Components/TestUtils.hs",
                    componentsSpecFile groupSpecs componentSpecs
                  ],
            -- Messages
            let messageSpecs = specMessages spec
             in mconcat
                  [ genHaskellDataFile "fix-spec/src/FIX/Messages/Class.hs",
                    messagesDataFiles messageSpecs,
                    genHaskellDataFile "fix-spec/src/FIX/Messages/Envelope.hs",
                    messagesGenFile messageSpecs,
                    genHaskellDataFile "fix-spec-gen/src/FIX/Messages/TestUtils.hs",
                    messagesSpecFile messageSpecs,
                    genHaskellDataFile "fix-spec-gen/test/FIX/Messages/EnvelopeSpec.hs",
                    topLevelMessagesFile messageSpecs,
                    genHaskellDataFile "fix-spec-gen/test/FIX/AnyMessageSpec.hs"
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
      componentComponents :: ComponentSpec -> Set Text
      componentComponents = foldMap pieceComponentNames . componentPieces
      messageComponents :: MessageSpec -> Set Text
      messageComponents = foldMap pieceComponentNames . messagePieces
      gatherComponentFixpoint :: Set Text -> Set Text
      gatherComponentFixpoint mentionedRoots =
        let cs = filter ((`S.member` mentionedRoots) . componentName) (specComponents spec)
            newCompoments = foldMap componentComponents cs
            newRoots = S.union mentionedRoots newCompoments
         in if newRoots == mentionedRoots then newRoots else gatherComponentFixpoint newRoots
      mentionedComponents :: Set Text
      mentionedComponents =
        gatherComponentFixpoint $
          S.unions
            [ foldMap pieceComponentNames (specHeader spec),
              foldMap pieceComponentNames (specTrailer spec),
              -- foldMap messageComponents filteredMessages
              foldMap messageComponents filteredMessages
            ]
      filteredCompoments = filter (\f -> S.member (componentName f) mentionedComponents) (specComponents spec)
      pieceFieldNames :: MessagePiece -> Set Text
      pieceFieldNames = \case
        MessagePieceField n _ -> S.singleton n
        MessagePieceComponent _ _ -> S.empty
        MessagePieceGroup gs _ -> S.insert (groupName gs) (foldMap pieceFieldNames (groupPieces gs))
      componentFields :: ComponentSpec -> Set Text
      componentFields = foldMap pieceFieldNames . componentPieces
      messageFields :: MessageSpec -> Set Text
      messageFields = foldMap pieceFieldNames . messagePieces
      mentionedFields =
        S.unions
          [ foldMap pieceFieldNames (specHeader spec),
            foldMap componentFields filteredCompoments,
            foldMap messageFields filteredMessages,
            foldMap pieceFieldNames (specTrailer spec)
          ]
      filteredFields = filter (\f -> S.member (fieldName f) mentionedFields) (specFields spec)
   in spec
        { specMessages = filteredMessages,
          specComponents = filteredCompoments,
          specFields = filteredFields
        }

-- Remove Length fields because we handle those automatically
foldDataFields :: Spec -> Spec
foldDataFields spec =
  Spec
    { specFields = otherFieldSpecs,
      specHeader = goMessagePieces (specHeader spec),
      specTrailer = goMessagePieces (specTrailer spec),
      specComponents = map goComponent (specComponents spec),
      specMessages = map goMessage (specMessages spec)
    }
  where
    (lengthFieldSpecs, otherFieldSpecs) = go (specFields spec)
      where
        go = \case
          [] -> ([], [])
          [fs] -> ([], [fs])
          (f : g : rest)
            | fieldType f == FieldTypeLength
                && fieldTypeIsData (fieldType g) ->
                let (ys, ns) = go rest
                 in (f : ys, g : ns)
            | otherwise ->
                let (ys, ns) = go (g : rest)
                 in (ys, f : ns)

    lengthFieldNames :: Set Text
    lengthFieldNames = S.fromList $ map fieldName lengthFieldSpecs
    isDataLengthFieldName :: Text -> Bool
    isDataLengthFieldName = (`S.member` lengthFieldNames)
    goMessagePieces :: [MessagePiece] -> [MessagePiece]
    goMessagePieces = mapMaybe goMessagePiece
    goMessagePiece :: MessagePiece -> Maybe MessagePiece
    goMessagePiece = \case
      MessagePieceComponent c r -> Just $ MessagePieceComponent c r
      MessagePieceGroup gs r -> Just $ MessagePieceGroup (goGroupSpec gs) r
      MessagePieceField f r -> do
        guard $ not $ isDataLengthFieldName f
        pure $ MessagePieceField f r
    goGroupSpec :: GroupSpec -> GroupSpec
    goGroupSpec gs = gs {groupPieces = goMessagePieces (groupPieces gs)}
    goComponent :: ComponentSpec -> ComponentSpec
    goComponent cs = cs {componentPieces = goMessagePieces (componentPieces cs)}
    goMessage :: MessageSpec -> MessageSpec
    goMessage ms = ms {messagePieces = goMessagePieces (messagePieces ms)}

-- Often a component only contains a group.
--
-- For example
-- @
-- <component name='InstrmtLegGrp'>
--  <group name='NoLegs' required='N'>
--   [...]
--  </group>
-- </component>
-- @
--
-- In this case we first simplify:
-- 1. Replace the group name by the component
-- 1. Remove the component
-- 2. Replacing all occurrences of the component by the group.
--
-- This helps us avoid naming conflicts, but may not always be enough.
-- TODO make sure we have a strategy for when it's not enough.
simplifyGroupComponents :: Spec -> Spec
simplifyGroupComponents spec =
  Spec
    { specFields = specFields spec,
      specHeader = goPieces (specHeader spec),
      specTrailer = goPieces (specTrailer spec),
      specComponents = map goComponent (specComponents spec),
      specMessages = map goMessage (specMessages spec)
    }
  where
    groupComponentMap =
      M.fromList $
        mapMaybe
          ( \ComponentSpec {..} -> case componentPieces of
              [MessagePieceGroup gs _] ->
                Just
                  ( componentName,
                    gs {groupName = componentName}
                  )
              _ -> Nothing
          )
          (specComponents spec)

    goMessage ms = ms {messagePieces = goPieces (messagePieces ms)}
    goComponent cs = cs {componentPieces = goPieces (componentPieces cs)}

    goPieces :: [MessagePiece] -> [MessagePiece]
    goPieces = map goPiece

    goPiece :: MessagePiece -> MessagePiece
    goPiece = \case
      mp@MessagePieceField {} -> mp
      mp@MessagePieceGroup {} -> mp
      mp@(MessagePieceComponent c r) -> case M.lookup c groupComponentMap of
        Nothing -> mp
        Just gs -> MessagePieceGroup gs r

-- @
-- If the repeating group is used, the first field of the repeating group
-- entry is required. In other words, it is conditionally required if the
-- NoXXX field is greater than zero.
-- @
makeFirstGroupElementsRequired :: Spec -> Spec
makeFirstGroupElementsRequired spec =
  Spec
    { specFields = specFields spec,
      specHeader = goPieces (specHeader spec),
      specTrailer = goPieces (specTrailer spec),
      specComponents = map goComponent (specComponents spec),
      specMessages = map goMessage (specMessages spec)
    }
  where
    goMessage ms = ms {messagePieces = goPieces (messagePieces ms)}
    goComponent cs = cs {componentPieces = goPieces (componentPieces cs)}
    goPieces = map goPiece
    goPiece = \case
      p@MessagePieceField {} -> p
      p@MessagePieceComponent {} -> p
      MessagePieceGroup gs r -> MessagePieceGroup (go gs) r

    go :: GroupSpec -> GroupSpec
    go gs =
      GroupSpec
        { groupName = groupName gs,
          groupNumberField = groupNumberField gs,
          groupPieces = goFirst (goPieces (groupPieces gs))
        }

    goFirst :: [MessagePiece] -> [MessagePiece]
    goFirst = \case
      [] -> []
      (p : ps) -> makeFirstRequired p : ps

    makeFirstRequired :: MessagePiece -> MessagePiece
    makeFirstRequired = \case
      MessagePieceField f _ -> MessagePieceField f True
      MessagePieceComponent c _ -> MessagePieceComponent c True
      MessagePieceGroup gs _ -> MessagePieceGroup gs True

-- Components of which all fields are optional cannot be parsed unambiguously
-- There is no way to render Just Nothing and Nothing render distinctly.
fixupNestedOptionals :: Spec -> Spec
fixupNestedOptionals spec =
  Spec
    { specFields = specFields spec,
      specHeader = goPieces (specHeader spec),
      specTrailer = goPieces (specTrailer spec),
      specComponents = map goComponent (specComponents spec),
      specMessages = map goMessage (specMessages spec)
    }
  where
    pieceAllOptional = \case
      MessagePieceField _ r -> not r
      MessagePieceGroup _ r -> not r
      MessagePieceComponent c r -> case find ((== c) . componentName) (specComponents spec) of
        Nothing -> not r
        Just cs -> piecesAllOptional (componentPieces cs)
    piecesAllOptional = all pieceAllOptional
    componentsWithOnlyOptionals :: Set Text
    componentsWithOnlyOptionals =
      S.fromList $
        map componentName $
          filter
            (piecesAllOptional . componentPieces)
            (specComponents spec)
    goMessage ms = ms {messagePieces = goPieces (messagePieces ms)}
    goComponent cs = cs {componentPieces = goPieces (componentPieces cs)}
    goGroup gs = gs {groupPieces = goPieces (groupPieces gs)}
    goPieces = map goPiece
    goPiece = \case
      p@MessagePieceField {} -> p
      MessagePieceGroup gs r -> MessagePieceGroup (goGroup gs) r
      MessagePieceComponent c r -> MessagePieceComponent c $ S.member c componentsWithOnlyOptionals || r

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
    let sections = flip map fieldSpecs $ \f ->
          let constructorName = fieldSpecConstructorName f
           in [ TH.pprint
                  [ genValidInstance constructorName
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
              "import FIX.Fields",
              "",
              "instance GenValid AnyField",
              ""
            ]
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
            [ [ "{-# LANGUAGE TypeApplications #-}",
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
        anyFieldSpecConstructorName fs = mkName (T.unpack ("Some" <> fieldName fs))
     in unlines $
          concat
            [ [ "{-# LANGUAGE DeriveGeneric #-}",
                "{-# LANGUAGE DerivingStrategies #-}",
                "{-# LANGUAGE LambdaCase #-}",
                "{-# LANGUAGE ScopedTypeVariables #-}",
                "module FIX.Fields (",
                "  AnyField(..),",
                "  anyFieldP,",
                "  anyFieldB,",
                "  IsAnyField(..),",
                "  module X",
                ") where",
                "",
                "import Data.ByteString (ByteString)",
                "import Data.Validity",
                "import Data.Void (Void)",
                "import FIX.Core",
                "import GHC.Generics (Generic)",
                "import Text.Megaparsec",
                "import Text.Megaparsec.Byte.Lexer",
                "import qualified Data.ByteString.Builder as ByteString",
                ""
              ],
              imports,
              [ TH.pprint
                  [ DataD
                      []
                      (mkName "AnyField")
                      []
                      Nothing
                      ( map
                          ( \fs ->
                              NormalC
                                (anyFieldSpecConstructorName fs)
                                [ ( Bang NoSourceUnpackedness SourceStrict,
                                    ConT (fieldSpecConstructorName fs)
                                  )
                                ]
                          )
                          fieldSpecs
                      )
                      [ DerivClause
                          (Just StockStrategy)
                          [ ConT (mkName "Show"),
                            ConT (mkName "Eq"),
                            ConT (mkName "Generic")
                          ]
                      ],
                    validityInstance (mkName "AnyField")
                  ],
                "anyFieldB :: AnyField -> ByteString.Builder",
                TH.pprint
                  [ FunD
                      (mkName "anyFieldB")
                      [ Clause
                          []
                          ( NormalB
                              ( LamCaseE
                                  ( map
                                      ( \fs ->
                                          let varName = mkName "f"
                                           in Match
                                                (ConP (anyFieldSpecConstructorName fs) [] [VarP varName])
                                                ( NormalB
                                                    (AppE (VarE (mkName "fieldB")) (VarE varName))
                                                )
                                                []
                                      )
                                      fieldSpecs
                                  )
                              )
                          )
                          []
                      ]
                  ],
                "anyFieldP :: Parsec Void ByteString AnyField",
                TH.pprint
                  [ FunD
                      (mkName "anyFieldP")
                      [ Clause
                          []
                          ( NormalB
                              ( DoE Nothing $
                                  let tagVarName = mkName "tag"
                                      helperName = mkName "fp"
                                   in concat
                                        [ [ BindS
                                              (VarP tagVarName)
                                              (VarE (mkName "decimal")),
                                            LetS
                                              [ SigD
                                                  helperName
                                                  ( let tyVarName = mkName "f"
                                                     in ForallT
                                                          [PlainTV tyVarName SpecifiedSpec]
                                                          [AppT (ConT (mkName "IsField")) (VarT tyVarName)]
                                                          ( AppT
                                                              ( AppT
                                                                  ( AppT
                                                                      (ConT (mkName "Parsec"))
                                                                      (ConT (mkName "Void"))
                                                                  )
                                                                  (ConT (mkName "ByteString"))
                                                              )
                                                              (VarT tyVarName)
                                                          )
                                                  ),
                                                FunD
                                                  helperName
                                                  [ Clause
                                                      []
                                                      ( NormalB
                                                          ( AppE
                                                              (VarE (mkName "fieldP"))
                                                              (VarE tagVarName)
                                                          )
                                                      )
                                                      []
                                                  ]
                                              ]
                                          ],
                                          [ NoBindS
                                              ( CaseE (VarE tagVarName) $
                                                  concat
                                                    [ map
                                                        ( \fs ->
                                                            Match
                                                              ( LitP
                                                                  ( IntegerL
                                                                      ( fromIntegral $
                                                                          ( if fieldTypeIsData (fieldType fs)
                                                                              then pred
                                                                              else id
                                                                          )
                                                                            (fieldNumber fs)
                                                                      )
                                                                  )
                                                              )
                                                              ( NormalB
                                                                  ( InfixE
                                                                      (Just (ConE (anyFieldSpecConstructorName fs)))
                                                                      (VarE (mkName "<$>"))
                                                                      (Just (VarE helperName))
                                                                  )
                                                              )
                                                              []
                                                        )
                                                        fieldSpecs,
                                                      [ Match
                                                          WildP
                                                          ( NormalB
                                                              ( AppE
                                                                  (VarE (mkName "fail"))
                                                                  ( InfixE
                                                                      (Just (LitE (StringL "Unknown field tag: ")))
                                                                      (VarE (mkName "<>"))
                                                                      (Just (AppE (VarE (mkName "show")) (VarE tagVarName)))
                                                                  )
                                                              )
                                                          )
                                                          []
                                                      ]
                                                    ]
                                              )
                                          ]
                                        ]
                              )
                          )
                          []
                      ]
                  ],
                "class (IsField a) => IsAnyField a where",
                "  unpackAnyField :: AnyField -> Maybe a",
                "  packAnyField :: a -> AnyField",
                TH.pprint $
                  map
                    ( \fs ->
                        InstanceD
                          Nothing
                          []
                          (AppT (ConT (mkName "IsAnyField")) (ConT (fieldSpecConstructorName fs)))
                          [ FunD
                              (mkName "packAnyField")
                              [Clause [] (NormalB (ConE (anyFieldSpecConstructorName fs))) []],
                            FunD
                              (mkName "unpackAnyField")
                              [ Clause
                                  []
                                  ( NormalB
                                      ( LamCaseE
                                          [ let varName = mkName "f"
                                             in Match
                                                  ( ConP
                                                      (anyFieldSpecConstructorName fs)
                                                      []
                                                      [VarP varName]
                                                  )
                                                  (NormalB (AppE (ConE (mkName "Just")) (VarE varName)))
                                                  [],
                                            Match WildP (NormalB (ConE (mkName "Nothing"))) []
                                          ]
                                      )
                                  )
                                  []
                              ]
                          ]
                    )
                    fieldSpecs
              ]
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
                      ( groupSpecGroupFieldName name gs,
                        Bang NoSourceUnpackedness SourceStrict,
                        if required
                          then AppT (ConT (mkName "NonEmpty")) (ConT (mkName (T.unpack (groupSpecConstructorName gs))))
                          else AppT ListT (ConT (mkName (T.unpack (groupSpecConstructorName gs))))
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

genValidInstance :: Name -> Dec
genValidInstance typeName =
  InstanceD
    Nothing
    []
    (AppT (ConT (mkName "GenValid")) (ConT typeName))
    [ FunD
        (mkName "genValid")
        [ Clause
            []
            ( NormalB
                (VarE (mkName "genValidStructurallyWithoutExtraChecking"))
            )
            []
        ],
      FunD
        (mkName "shrinkValid")
        [ Clause
            []
            ( NormalB
                (VarE (mkName "shrinkValidStructurallyWithoutExtraFiltering"))
            )
            []
        ]
    ]

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
                  (VarE (groupSpecGroupFieldName name gs))
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
                    (VarE (mkName "mconcat"))
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
                  (VarP (groupSpecGroupFieldName name gs))
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
  -- TODO results will be wrong if the same group name is generated twice.
  S.toList $
    mconcat
      [ foldMap pieceGroupSpecs specHeader,
        foldMap pieceGroupSpecs specTrailer,
        foldMap pieceGroupSpecs $ foldMap componentPieces specComponents,
        foldMap pieceGroupSpecs $ foldMap messagePieces specMessages
      ]

groupSpecConstructorName :: GroupSpec -> Text
groupSpecConstructorName = (<> "GroupElem") . (\t -> fromMaybe t $ T.stripPrefix "No" t) . groupName

groupSpecGroupFieldName :: Text -> GroupSpec -> Name
groupSpecGroupFieldName name gs = mkName $ lowerHead $ T.unpack $ (name <>) . (<> "Group") . (\t -> fromMaybe t $ T.stripPrefix "No" t) $ groupName gs

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
                    messagePiecesIsComponentInstance constructorName groupPieces,
                    InstanceD
                      Nothing
                      []
                      (AppT (ConT (mkName "IsGroupElement")) (ConT (mkName (T.unpack constructorName))))
                      [ TySynD
                          (mkName "GroupNumField")
                          [PlainTV (mkName (T.unpack constructorName)) ()]
                          (ConT (mkName (T.unpack groupNumberField))),
                        FunD
                          (mkName "mkGroupNum")
                          [ Clause
                              [VarP (mkName "Proxy")]
                              (NormalB (ConE (mkName (T.unpack groupNumberField))))
                              []
                          ],
                        FunD
                          (mkName "countGroupNum")
                          [ Clause
                              [VarP (mkName "Proxy")]
                              (NormalB (ConE (mkName ("un" <> T.unpack groupNumberField))))
                              []
                          ]
                      ]
                  ]
              ]
         in unlines $
              concat
                [ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}",
                    "{-# LANGUAGE DeriveGeneric #-}",
                    "{-# LANGUAGE DerivingStrategies #-}",
                    "{-# LANGUAGE MultiParamTypeClasses #-}",
                    "{-# LANGUAGE RecordWildCards #-}",
                    "{-# LANGUAGE TypeFamilies #-}",
                    "",
                    "module FIX.Groups." <> T.unpack constructorName <> " where",
                    "",
                    "import Data.Validity",
                    "import FIX.Groups.Class",
                    "import FIX.Components.Class",
                    "import FIX.Fields.MsgType",
                    "import GHC.Generics (Generic)",
                    "import Data.Proxy",
                    "",
                    "import FIX.Fields." <> T.unpack groupNumberField,
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
                "import Data.List.NonEmpty (NonEmpty)",
                "import FIX.Groups.Class",
                "import FIX.Components.Class",
                "import FIX.Fields.MsgType",
                "import GHC.Generics (Generic)",
                "import Data.Proxy",
                ""
              ],
              messagePiecesImports componentPieces,
              section
            ]

componentsGenFile :: [GroupSpec] -> [ComponentSpec] -> CodeGen
componentsGenFile groupSpecs componentSpecs =
  genHaskellFile "fix-spec-gen/src/FIX/Components/Gen.hs" $
    let componentsImports =
          map
            ( \f ->
                "import FIX.Components." <> T.unpack (componentName f)
            )
            componentSpecs
        groupsImports =
          map
            ( \f ->
                "import FIX.Groups." <> T.unpack (groupSpecConstructorName f)
            )
            groupSpecs
        componentsSections = flip map componentSpecs $ \f ->
          let constructorName = componentSpecConstructorName f
           in [ TH.pprint
                  [ genValidInstance constructorName
                  ]
              ]
        groupsSections = flip map groupSpecs $ \f ->
          let constructorName = groupSpecConstructorName f
           in [ TH.pprint
                  [ genValidInstance $ mkName $ T.unpack constructorName
                  ]
              ]
     in unlines $
          concat $
            [ "{-# OPTIONS_GHC -Wno-orphans #-}",
              "",
              "module FIX.Components.Gen where",
              "",
              "import Data.GenValidity",
              "import Data.GenValidity.ByteString ()",
              "import FIX.Fields.Gen ()",
              ""
            ]
              : componentsImports
              : groupsImports
              : groupsSections
              ++ componentsSections

componentsSpecFile :: [GroupSpec] -> [ComponentSpec] -> CodeGen
componentsSpecFile groupSpecs componentSpecs =
  genHaskellFile "fix-spec-gen/test/FIX/ComponentsSpec.hs" $
    let componentsImports =
          map
            ( \f ->
                "import FIX.Components." <> T.unpack (componentName f)
            )
            componentSpecs
        groupsImports =
          map
            ( \f ->
                "import FIX.Groups." <> T.unpack (groupSpecConstructorName f)
            )
            groupSpecs

        groupStatements = flip map groupSpecs $ \f ->
          let constructorName = groupSpecConstructorName f
              conType = ConT (mkName (T.unpack constructorName))
              nameLitE = LitE (StringL (T.unpack (groupName f)))
           in NoBindS
                ( AppE
                    (AppE (VarE (mkName "describe")) nameLitE)
                    ( DoE
                        Nothing
                        [ NoBindS
                            (AppTypeE (VarE (mkName "genValidSpec")) conType),
                          NoBindS
                            (AppTypeE (VarE (mkName "componentSpec")) conType),
                          NoBindS
                            (AppTypeE (VarE (mkName "groupSpec")) conType)
                        ]
                    )
                )
        componentStatements = flip map componentSpecs $ \f ->
          let constructorName = componentSpecConstructorName f
              nameLitE = LitE (StringL (T.unpack (componentName f)))
           in NoBindS
                ( AppE
                    (AppE (VarE (mkName "describe")) nameLitE)
                    ( DoE
                        Nothing
                        [ NoBindS
                            (AppTypeE (VarE (mkName "genValidSpec")) (ConT constructorName)),
                          NoBindS
                            (AppTypeE (VarE (mkName "componentSpec")) (ConT constructorName))
                        ]
                    )
                )
        statements = groupStatements <> componentStatements
     in unlines $
          concat
            [ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}",
                "{-# LANGUAGE TypeApplications #-}",
                "",
                "module FIX.ComponentsSpec where",
                "",
                "import FIX.Groups.TestUtils",
                "import FIX.Components.TestUtils",
                "import FIX.Components.Gen ()",
                "import Test.Syd",
                "import Test.Syd.Validity",
                ""
              ],
              groupsImports,
              componentsImports,
              [ TH.pprint
                  [ SigD (mkName "spec") (ConT (mkName "Spec")),
                    FunD
                      (mkName "spec")
                      [ Clause
                          []
                          ( NormalB
                              ( if null statements
                                  then AppE (VarE (mkName "pure")) (VarE (mkName "()"))
                                  else DoE Nothing statements
                              )
                          )
                          []
                      ]
                  ]
              ]
            ]

messageTypeConstructorName :: MessageSpec -> Name
messageTypeConstructorName ms = mkName ("MsgType" <> upperHead (T.unpack (messageName ms)))

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
                          (NormalB (ConE (messageTypeConstructorName f)))
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
    let sections = flip map messageSpecs $ \f ->
          let constructorName = messageSpecConstructorName f
           in [ TH.pprint
                  [ genValidInstance constructorName
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
              "import FIX.Components.Gen ()",
              "import FIX.Messages.Header",
              "import FIX.Messages.Trailer",
              "import FIX.Messages.Envelope",
              "import FIX.Messages",
              "",
              "instance GenValid Header",
              "instance GenValid Trailer",
              "instance (GenValid a) => GenValid (Envelope a)",
              "instance GenValid AnyMessage",
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
              nameLitE = LitE (StringL (T.unpack (messageName f)))
           in NoBindS
                ( AppE
                    (AppE (VarE (mkName "describe")) nameLitE)
                    ( DoE
                        Nothing
                        [ NoBindS
                            (AppTypeE (VarE (mkName "genValidSpec")) (ConT constructorName)),
                          NoBindS
                            (AppTypeE (VarE (mkName "componentSpec")) (ConT constructorName)),
                          NoBindS
                            ( AppE
                                (AppTypeE (VarE (mkName "messageSpec")) (ConT constructorName))
                                nameLitE
                            )
                        ]
                    )
                )
     in unlines $
          concat
            [ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
                "{-# LANGUAGE TypeApplications #-}",
                "",
                "module FIX.MessagesSpec where",
                "",
                "import FIX.Components.TestUtils",
                "import FIX.Messages.TestUtils",
                "import FIX.Messages.Gen ()",
                "import Test.Syd",
                "import Test.Syd.Validity",
                ""
              ],
              imports,
              [ TH.pprint
                  [ SigD (mkName "spec") (ConT (mkName "Spec")),
                    FunD (mkName "spec") [Clause [] (NormalB (DoE Nothing statements)) []]
                  ]
              ]
            ]

topLevelMessagesFile :: [MessageSpec] -> CodeGen
topLevelMessagesFile messageSpecs =
  genHaskellFile "fix-spec/src/FIX/Messages.hs" $
    let imports =
          map
            ( \f ->
                "import FIX.Messages." <> T.unpack (messageName f) <> " as X"
            )
            messageSpecs
        anyMessageSpecConstructorName fs = mkName (T.unpack ("Some" <> messageName fs))
     in unlines $
          concat
            [ [ "{-# LANGUAGE DeriveGeneric #-}",
                "{-# LANGUAGE DerivingStrategies #-}",
                "{-# LANGUAGE LambdaCase #-}",
                "{-# LANGUAGE RecordWildCards #-}",
                "{-# LANGUAGE ScopedTypeVariables #-}",
                "module FIX.Messages (",
                "  AnyMessage(..),",
                "  anyMessageType,",
                "  anyMessageB,",
                "  anyMessageP,",
                "  IsAnyMessage(..),",
                "  module X",
                ") where",
                "",
                "import Data.ByteString (ByteString)",
                "import Data.Validity",
                "import Data.Void (Void)",
                "import FIX.Fields",
                "import FIX.Messages.Class",
                "import FIX.Messages.Envelope",
                "import GHC.Generics (Generic)",
                "import Text.Megaparsec",
                "import qualified Data.ByteString.Builder as ByteString",
                ""
              ],
              imports,
              [ TH.pprint
                  [ DataD
                      []
                      (mkName "AnyMessage")
                      []
                      Nothing
                      ( map
                          ( \fs ->
                              NormalC
                                (anyMessageSpecConstructorName fs)
                                [ ( Bang NoSourceUnpackedness SourceStrict,
                                    ConT (messageSpecConstructorName fs)
                                  )
                                ]
                          )
                          messageSpecs
                      )
                      [ DerivClause
                          (Just StockStrategy)
                          [ ConT (mkName "Show"),
                            ConT (mkName "Eq"),
                            ConT (mkName "Generic")
                          ]
                      ],
                    validityInstance (mkName "AnyMessage")
                  ],
                "anyMessageType :: AnyMessage -> MsgType",
                TH.pprint
                  [ FunD
                      (mkName "anyMessageType")
                      [ Clause
                          []
                          ( NormalB
                              ( LamCaseE
                                  ( map
                                      ( \ms ->
                                          Match
                                            ( ConP
                                                (anyMessageSpecConstructorName ms)
                                                []
                                                [WildP]
                                            )
                                            (NormalB (ConE (messageTypeConstructorName ms)))
                                            []
                                      )
                                      messageSpecs
                                  )
                              )
                          )
                          []
                      ]
                  ],
                "anyMessageB :: Envelope AnyMessage -> ByteString.Builder",
                TH.pprint
                  [ FunD
                      (mkName "anyMessageB")
                      [ Clause
                          [ConP (mkName "Envelope{..}") [] []]
                          ( NormalB
                              ( let helperName = mkName "mb"
                                    varName = mkName "f"
                                 in LetE
                                      [ SigD
                                          helperName
                                          ( let tyVarName = mkName "a"
                                             in ForallT
                                                  [PlainTV tyVarName SpecifiedSpec]
                                                  [AppT (ConT (mkName "IsMessage")) (VarT tyVarName)]
                                                  ( InfixT
                                                      (VarT tyVarName)
                                                      (mkName "->")
                                                      (ConT (mkName "ByteString.Builder"))
                                                  )
                                          ),
                                        FunD
                                          helperName
                                          [ Clause
                                              []
                                              ( NormalB
                                                  ( AppE
                                                      ( AppE
                                                          (VarE (mkName "messageB"))
                                                          (VarE (mkName "envelopeHeader"))
                                                      )
                                                      (VarE (mkName "envelopeTrailer"))
                                                  )
                                              )
                                              []
                                          ]
                                      ]
                                      ( CaseE
                                          (VarE (mkName "envelopeContents"))
                                          ( map
                                              ( \fs ->
                                                  Match
                                                    (ConP (anyMessageSpecConstructorName fs) [] [VarP varName])
                                                    ( NormalB
                                                        ( AppE
                                                            (VarE helperName)
                                                            (VarE varName)
                                                        )
                                                    )
                                                    []
                                              )
                                              messageSpecs
                                          )
                                      )
                              )
                          )
                          []
                      ]
                  ],
                "anyMessageP :: Parsec Void ByteString (Envelope AnyMessage)",
                TH.pprint
                  [ FunD
                      (mkName "anyMessageP")
                      [ Clause
                          []
                          ( NormalB
                              ( DoE Nothing $
                                  let bsVarName = mkName "bs"
                                      blVarName = mkName "bl"
                                      typVarName = mkName "typ"
                                      helperName = mkName "mp"
                                   in concat
                                        [ [ BindS
                                              (ConP (mkName "SomeBeginString") [] [VarP bsVarName])
                                              (VarE (mkName "anyFieldP")),
                                            BindS
                                              (ConP (mkName "SomeBodyLength") [] [VarP blVarName])
                                              (VarE (mkName "anyFieldP")),
                                            BindS
                                              (ConP (mkName "SomeMsgType") [] [VarP typVarName])
                                              (VarE (mkName "anyFieldP")),
                                            LetS
                                              [ SigD
                                                  helperName
                                                  ( let tyVarName = mkName "f"
                                                     in ForallT
                                                          [PlainTV tyVarName SpecifiedSpec]
                                                          [AppT (ConT (mkName "IsMessage")) (VarT tyVarName)]
                                                          ( AppT
                                                              ( AppT
                                                                  ( AppT
                                                                      (ConT (mkName "Parsec"))
                                                                      (ConT (mkName "Void"))
                                                                  )
                                                                  (ConT (mkName "ByteString"))
                                                              )
                                                              ( AppT
                                                                  (ConT (mkName "Envelope"))
                                                                  (VarT tyVarName)
                                                              )
                                                          )
                                                  ),
                                                FunD
                                                  helperName
                                                  [ Clause
                                                      []
                                                      ( NormalB
                                                          ( AppE
                                                              ( AppE
                                                                  ( AppE
                                                                      (VarE (mkName "messageP"))
                                                                      (VarE bsVarName)
                                                                  )
                                                                  (VarE blVarName)
                                                              )
                                                              (VarE typVarName)
                                                          )
                                                      )
                                                      []
                                                  ]
                                              ]
                                          ],
                                          [ NoBindS
                                              ( CaseE (VarE typVarName) $
                                                  concat
                                                    [ map
                                                        ( \fs ->
                                                            Match
                                                              (ConP (messageTypeConstructorName fs) [] [])
                                                              ( NormalB
                                                                  ( InfixE
                                                                      ( Just
                                                                          ( AppE
                                                                              (VarE (mkName "fmap"))
                                                                              (ConE (anyMessageSpecConstructorName fs))
                                                                          )
                                                                      )
                                                                      (VarE (mkName "<$>"))
                                                                      (Just (VarE helperName))
                                                                  )
                                                              )
                                                              []
                                                        )
                                                        messageSpecs,
                                                      [ Match
                                                          WildP
                                                          ( NormalB
                                                              ( AppE
                                                                  (VarE (mkName "fail"))
                                                                  ( InfixE
                                                                      (Just (LitE (StringL "Unknown message tag: ")))
                                                                      (VarE (mkName "<>"))
                                                                      (Just (AppE (VarE (mkName "show")) (VarE typVarName)))
                                                                  )
                                                              )
                                                          )
                                                          []
                                                      ]
                                                    ]
                                              )
                                          ]
                                        ]
                              )
                          )
                          []
                      ]
                  ],
                "class (IsMessage a) => IsAnyMessage a where",
                "  unpackAnyMessage :: AnyMessage -> Maybe a",
                "  packAnyMessage :: a -> AnyMessage",
                TH.pprint $
                  map
                    ( \fs ->
                        InstanceD
                          Nothing
                          []
                          (AppT (ConT (mkName "IsAnyMessage")) (ConT (messageSpecConstructorName fs)))
                          [ FunD
                              (mkName "packAnyMessage")
                              [Clause [] (NormalB (ConE (anyMessageSpecConstructorName fs))) []],
                            FunD
                              (mkName "unpackAnyMessage")
                              [ Clause
                                  []
                                  ( NormalB
                                      ( LamCaseE
                                          [ let varName = mkName "f"
                                             in Match
                                                  ( ConP
                                                      (anyMessageSpecConstructorName fs)
                                                      []
                                                      [VarP varName]
                                                  )
                                                  (NormalB (AppE (ConE (mkName "Just")) (VarE varName)))
                                                  [],
                                            Match WildP (NormalB (ConE (mkName "Nothing"))) []
                                          ]
                                      )
                                  )
                                  []
                              ]
                          ]
                    )
                    messageSpecs
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
