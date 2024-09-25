{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.UnderlyingStipulationsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoUnderlyingStips
import FIX.Fields.UnderlyingStipType
import FIX.Fields.UnderlyingStipValue
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "UnderlyingStipulations"
--   , groupNumberField = "NoUnderlyingStips"
--   , groupPieces =
--       [ MessagePieceField "UnderlyingStipType" True
--       , MessagePieceField "UnderlyingStipValue" False
--       ]
--   }
data UnderlyingStipulationsGroupElem = UnderlyingStipulationsGroupElem
  { underlyingStipulationsGroupElemUnderlyingStipType :: !UnderlyingStipType,
    underlyingStipulationsGroupElemUnderlyingStipValue :: !(Maybe UnderlyingStipValue)
  }
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingStipulationsGroupElem

instance IsComponent UnderlyingStipulationsGroupElem where
  toComponentFields ((UnderlyingStipulationsGroupElem {..})) =
    mconcat
      [ requiredFieldB underlyingStipulationsGroupElemUnderlyingStipType,
        optionalFieldB underlyingStipulationsGroupElemUnderlyingStipValue
      ]
  fromComponentFields = do
    underlyingStipulationsGroupElemUnderlyingStipType <- requiredFieldP
    underlyingStipulationsGroupElemUnderlyingStipValue <- optionalFieldP
    pure (UnderlyingStipulationsGroupElem {..})

instance IsGroupElement UnderlyingStipulationsGroupElem where
  type GroupNumField UnderlyingStipulationsGroupElem = NoUnderlyingStips
  mkGroupNum Proxy = NoUnderlyingStips
  countGroupNum Proxy = unNoUnderlyingStips

makeUnderlyingStipulationsGroupElem :: UnderlyingStipType -> UnderlyingStipulationsGroupElem
makeUnderlyingStipulationsGroupElem underlyingStipulationsGroupElemUnderlyingStipType =
  let underlyingStipulationsGroupElemUnderlyingStipValue = Nothing
   in (UnderlyingStipulationsGroupElem {..})