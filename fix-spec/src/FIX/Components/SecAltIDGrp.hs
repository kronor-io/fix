{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.SecAltIDGrp where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Groups.Class
import FIX.Groups.SecurityAltIDGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "SecAltIDGrp"
--   , componentPieces =
--       [ MessagePieceGroup
--           GroupSpec
--             { groupName = "NoSecurityAltID"
--             , groupPieces =
--                 [ MessagePieceField "SecurityAltID" True
--                 , MessagePieceField "SecurityAltIDSource" False
--                 ]
--             }
--           False
--       ]
--   }
data SecAltIDGrp = SecAltIDGrp {secAltIDGrpSecurityAltIDGroup :: ![SecurityAltIDGroupElem]}
  deriving stock (Show, Eq, Generic)

instance Validity SecAltIDGrp

instance IsComponent SecAltIDGrp where
  toComponentFields ((SecAltIDGrp {..})) = mconcat [optionalGroupB secAltIDGrpSecurityAltIDGroup]
  fromComponentFields = do
    secAltIDGrpSecurityAltIDGroup <- optionalGroupP
    pure (SecAltIDGrp {..})