{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.SecurityDefinitionRequestUnderlyingsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Components.UnderlyingInstrument
import FIX.Fields.MsgType
import FIX.Fields.NoUnderlyings
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "SecurityDefinitionRequestUnderlyings"
--   , groupNumberField = "NoUnderlyings"
--   , groupPieces =
--       [ MessagePieceComponent "UnderlyingInstrument" True ]
--   }
data SecurityDefinitionRequestUnderlyingsGroupElem = SecurityDefinitionRequestUnderlyingsGroupElem {securityDefinitionRequestUnderlyingsGroupElemUnderlyingInstrument :: !UnderlyingInstrument}
  deriving stock (Show, Eq, Generic)

instance Validity SecurityDefinitionRequestUnderlyingsGroupElem

instance IsComponent SecurityDefinitionRequestUnderlyingsGroupElem where
  toComponentFields ((SecurityDefinitionRequestUnderlyingsGroupElem {..})) = mconcat [requiredComponentB securityDefinitionRequestUnderlyingsGroupElemUnderlyingInstrument]
  fromComponentFields = do
    securityDefinitionRequestUnderlyingsGroupElemUnderlyingInstrument <- requiredComponentP
    pure (SecurityDefinitionRequestUnderlyingsGroupElem {..})

instance IsGroupElement SecurityDefinitionRequestUnderlyingsGroupElem where
  type GroupNumField SecurityDefinitionRequestUnderlyingsGroupElem = NoUnderlyings
  mkGroupNum Proxy = NoUnderlyings
  countGroupNum Proxy = unNoUnderlyings

makeSecurityDefinitionRequestUnderlyingsGroupElem :: UnderlyingInstrument -> SecurityDefinitionRequestUnderlyingsGroupElem
makeSecurityDefinitionRequestUnderlyingsGroupElem securityDefinitionRequestUnderlyingsGroupElemUnderlyingInstrument =
  let
   in (SecurityDefinitionRequestUnderlyingsGroupElem {..})
