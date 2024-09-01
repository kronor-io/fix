{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.UnderlyingStipsGroupElem where

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
--   { groupName = "NoUnderlyingStips"
--   , groupPieces =
--       [ MessagePieceField "UnderlyingStipType" True
--       , MessagePieceField "UnderlyingStipValue" False
--       ]
--   }
data UnderlyingStipsGroupElem = UnderlyingStipsGroupElem
  { underlyingStipsGroupElemUnderlyingStipType :: !UnderlyingStipType,
    underlyingStipsGroupElemUnderlyingStipValue :: !(Maybe UnderlyingStipValue)
  }
  deriving stock (Show, Eq, Generic)

instance Validity UnderlyingStipsGroupElem

instance IsComponent UnderlyingStipsGroupElem where
  toComponentFields ((UnderlyingStipsGroupElem {..})) =
    mconcat
      [ requiredFieldB underlyingStipsGroupElemUnderlyingStipType,
        optionalFieldB underlyingStipsGroupElemUnderlyingStipValue
      ]
  fromComponentFields = do
    underlyingStipsGroupElemUnderlyingStipType <- requiredFieldP
    underlyingStipsGroupElemUnderlyingStipValue <- optionalFieldP
    pure (UnderlyingStipsGroupElem {..})

instance IsGroupElement UnderlyingStipsGroupElem where
  type GroupNumField UnderlyingStipsGroupElem = NoUnderlyingStips
  mkGroupNum Proxy = NoUnderlyingStips
  countGroupNum Proxy = unNoUnderlyingStips
