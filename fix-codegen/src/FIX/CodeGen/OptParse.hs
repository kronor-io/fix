{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen.OptParse
  ( Settings (..),
    getSettings,
  )
where

import OptEnvConf
import Path
import Path.IO
import Paths_fix_codegen (version)

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
