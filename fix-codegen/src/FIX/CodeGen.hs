{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen (runFixCodeGen) where

import FIX.CodeGen.Spec
import OptEnvConf
import Path
import Path.IO
import Paths_fix_codegen (version)
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
