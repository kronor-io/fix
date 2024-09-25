{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.UnderlyingSecurityAltIDGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoUnderlyingSecurityAltID
import FIX.Fields.UnderlyingSecurityAltID
import FIX.Fields.UnderlyingSecurityAltIDSource
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "UnderlyingSecurityAltID"
--   , groupNumberField = "NoUnderlyingSecurityAltID"
--   , groupPieces =
--       [ MessagePieceField "UnderlyingSecurityAltID" True
--       , MessagePieceField "UnderlyingSecurityAltIDSource" False
--       ]
--   }
data UnderlyingSecurityAltIDGroupElem = UnderlyingSecurityAltIDGroupElem
  { underlyingSecurityAltIDGroupElemUnderlyingSecurityAltID :: !UnderlyingSecurityAltID,
    underlyingSecurityAltIDGroupElemUnderlyingSecurityAltIDSource :: !(Maybe UnderlyingSecurityAltIDSource)
  }
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingSecurityAltIDGroupElem

instance IsComponent UnderlyingSecurityAltIDGroupElem where
  toComponentFields ((UnderlyingSecurityAltIDGroupElem {..})) =
    mconcat
      [ requiredFieldB underlyingSecurityAltIDGroupElemUnderlyingSecurityAltID,
        optionalFieldB underlyingSecurityAltIDGroupElemUnderlyingSecurityAltIDSource
      ]
  fromComponentFields = do
    underlyingSecurityAltIDGroupElemUnderlyingSecurityAltID <- requiredFieldP
    underlyingSecurityAltIDGroupElemUnderlyingSecurityAltIDSource <- optionalFieldP
    pure (UnderlyingSecurityAltIDGroupElem {..})

instance IsGroupElement UnderlyingSecurityAltIDGroupElem where
  type GroupNumField UnderlyingSecurityAltIDGroupElem = NoUnderlyingSecurityAltID
  mkGroupNum Proxy = NoUnderlyingSecurityAltID
  countGroupNum Proxy = unNoUnderlyingSecurityAltID

makeUnderlyingSecurityAltIDGroupElem :: UnderlyingSecurityAltID -> UnderlyingSecurityAltIDGroupElem
makeUnderlyingSecurityAltIDGroupElem underlyingSecurityAltIDGroupElemUnderlyingSecurityAltID =
  let underlyingSecurityAltIDGroupElemUnderlyingSecurityAltIDSource = Nothing
   in (UnderlyingSecurityAltIDGroupElem {..})
