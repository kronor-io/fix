{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import Control.Monad
import qualified Data.Text as T
import FIX.CodeGen.OptParse
import FIX.CodeGen.Spec
import Language.Haskell.TH as TH
import Path
import System.Exit
import Text.XML as XML

runFixCodeGen :: IO ()
runFixCodeGen = do
  settings <- getSettings
  doc <- XML.readFile def (fromAbsFile (settingsSpecFile settings))
  case parseSpec doc of
    Nothing -> die "Failed to parse specfication."
    Just spec -> do
      forM_ (specFields spec) $ \f@FieldSpec {..} -> do
        putStrLn ""

        putStrLn $ "-- " <> show f

        let constructorName = mkName $ T.unpack fieldName
        let selectorName = mkName $ "un" <> T.unpack fieldName
        let typ = case fieldType of
              FieldTypeBoolean -> ConT (mkName "Bool")
              _ -> ConT (mkName "ByteString")
        putStrLn $
          TH.pprint $
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
        putStrLn $
          TH.pprint $
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
                          ( InfixE
                              (Just (VarE (mkName "toValue")))
                              (VarE (mkName "."))
                              (Just (VarE selectorName))
                          )
                      )
                      []
                  ],
                FunD
                  (mkName "fieldToValue")
                  [ Clause
                      []
                      ( NormalB
                          ( InfixE
                              (Just (VarE (mkName "fromValue")))
                              (VarE (mkName ">=>"))
                              ( Just
                                  ( InfixE
                                      (Just (VarE (mkName "prettyValidate")))
                                      (VarE (mkName "."))
                                      (Just (VarE constructorName))
                                  )
                              )
                          )
                      )
                      []
                  ]
              ]
        putStrLn $
          TH.pprint $
            InstanceD
              Nothing
              []
              (AppT (ConT (mkName "Validity")) (ConT constructorName))
              []

        putStrLn ""
        pure ()
