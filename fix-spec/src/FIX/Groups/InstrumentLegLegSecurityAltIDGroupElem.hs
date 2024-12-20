{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.InstrumentLegLegSecurityAltIDGroupElem where

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
--   { groupName = "InstrumentLegLegSecurityAltID"
--   , groupNumberField = "NoLegSecurityAltID"
--   , groupPieces =
--       [ MessagePieceField "LegSecurityAltID" True
--       , MessagePieceField "LegSecurityAltIDSource" False
--       ]
--   }
data InstrumentLegLegSecurityAltIDGroupElem = InstrumentLegLegSecurityAltIDGroupElem
  { instrumentLegLegSecurityAltIDGroupElemLegSecurityAltID :: !LegSecurityAltID,
    instrumentLegLegSecurityAltIDGroupElemLegSecurityAltIDSource :: !(Maybe LegSecurityAltIDSource)
  }
  deriving stock (Show, Eq, Generic)

instance Validity InstrumentLegLegSecurityAltIDGroupElem

instance IsComponent InstrumentLegLegSecurityAltIDGroupElem where
  toComponentFields ((InstrumentLegLegSecurityAltIDGroupElem {..})) =
    mconcat
      [ requiredFieldB instrumentLegLegSecurityAltIDGroupElemLegSecurityAltID,
        optionalFieldB instrumentLegLegSecurityAltIDGroupElemLegSecurityAltIDSource
      ]
  fromComponentFields = do
    instrumentLegLegSecurityAltIDGroupElemLegSecurityAltID <- requiredFieldP
    instrumentLegLegSecurityAltIDGroupElemLegSecurityAltIDSource <- optionalFieldP
    pure (InstrumentLegLegSecurityAltIDGroupElem {..})

instance IsGroupElement InstrumentLegLegSecurityAltIDGroupElem where
  type GroupNumField InstrumentLegLegSecurityAltIDGroupElem = NoLegSecurityAltID
  mkGroupNum Proxy = NoLegSecurityAltID
  countGroupNum Proxy = unNoLegSecurityAltID

makeInstrumentLegLegSecurityAltIDGroupElem :: LegSecurityAltID -> InstrumentLegLegSecurityAltIDGroupElem
makeInstrumentLegLegSecurityAltIDGroupElem instrumentLegLegSecurityAltIDGroupElemLegSecurityAltID =
  let instrumentLegLegSecurityAltIDGroupElemLegSecurityAltIDSource = Nothing
   in (InstrumentLegLegSecurityAltIDGroupElem {..})
