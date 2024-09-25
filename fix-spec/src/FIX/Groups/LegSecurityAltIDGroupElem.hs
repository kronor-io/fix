{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.LegSecurityAltIDGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.LegSecurityAltID
import FIX.Fields.LegSecurityAltIDSource
import FIX.Fields.MsgType
import FIX.Fields.NoLegSecurityAltID
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "LegSecurityAltID"
--   , groupNumberField = "NoLegSecurityAltID"
--   , groupPieces =
--       [ MessagePieceField "LegSecurityAltID" True
--       , MessagePieceField "LegSecurityAltIDSource" False
--       ]
--   }
data LegSecurityAltIDGroupElem = LegSecurityAltIDGroupElem
  { legSecurityAltIDGroupElemLegSecurityAltID :: !LegSecurityAltID,
    legSecurityAltIDGroupElemLegSecurityAltIDSource :: !(Maybe LegSecurityAltIDSource)
  }
  deriving stock (Show, Eq, Generic)

instance Validity LegSecurityAltIDGroupElem

instance IsComponent LegSecurityAltIDGroupElem where
  toComponentFields ((LegSecurityAltIDGroupElem {..})) =
    mconcat
      [ requiredFieldB legSecurityAltIDGroupElemLegSecurityAltID,
        optionalFieldB legSecurityAltIDGroupElemLegSecurityAltIDSource
      ]
  fromComponentFields = do
    legSecurityAltIDGroupElemLegSecurityAltID <- requiredFieldP
    legSecurityAltIDGroupElemLegSecurityAltIDSource <- optionalFieldP
    pure (LegSecurityAltIDGroupElem {..})

instance IsGroupElement LegSecurityAltIDGroupElem where
  type GroupNumField LegSecurityAltIDGroupElem = NoLegSecurityAltID
  mkGroupNum Proxy = NoLegSecurityAltID
  countGroupNum Proxy = unNoLegSecurityAltID

makeLegSecurityAltIDGroupElem :: LegSecurityAltID -> LegSecurityAltIDGroupElem
makeLegSecurityAltIDGroupElem legSecurityAltIDGroupElemLegSecurityAltID =
  let legSecurityAltIDGroupElemLegSecurityAltIDSource = Nothing
   in (LegSecurityAltIDGroupElem {..})
