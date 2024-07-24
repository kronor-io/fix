{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module FIX.CodeGen (runFixCodeGen) where

import FIX.CodeGen.OptParse
import FIX.CodeGen.Spec
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
      mapM_ print $ specFields spec
