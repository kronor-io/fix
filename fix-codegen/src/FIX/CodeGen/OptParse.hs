{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FIX.CodeGen.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Data.Set (Set)
import Data.Text (Text)
import OptEnvConf
import Path
import Path.IO
import Paths_fix_codegen (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "FIX protocol code generation"

data Settings = Settings
  { settingSpecFile :: !(Path Abs File),
    settingOutputDir :: !(Path Abs Dir),
    settingMessages :: !(Maybe (Set Text))
  }

instance HasParser Settings where
  settingsParser = withLocalYamlConfig $ do
    settingSpecFile <-
      filePathSetting
        [ help "Path to a FIX specificaton file",
          env "FIX_SPEC_FILE"
        ]
    settingOutputDir <-
      directoryPathSetting
        [ help "Path to the top-level directory of this monorepo output dir",
          name "output",
          value "."
        ]
    settingMessages <-
      optional $
        setting
          [ help "Messages to generate code for",
            conf "messages"
          ]
    pure Settings {..}
