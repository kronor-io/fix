{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.NewOrderMultilegCustomFieldsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.CustomFieldsContext
import FIX.Fields.CustomFieldsName
import FIX.Fields.CustomFieldsValue
import FIX.Fields.MsgType
import FIX.Fields.NoCustomFields
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "NewOrderMultilegCustomFields"
--   , groupNumberField = "NoCustomFields"
--   , groupPieces =
--       [ MessagePieceField "CustomFieldsName" True
--       , MessagePieceField "CustomFieldsValue" False
--       , MessagePieceField "CustomFieldsContext" False
--       ]
--   }
data NewOrderMultilegCustomFieldsGroupElem = NewOrderMultilegCustomFieldsGroupElem
  { newOrderMultilegCustomFieldsGroupElemCustomFieldsName :: !CustomFieldsName,
    newOrderMultilegCustomFieldsGroupElemCustomFieldsValue :: !(Maybe CustomFieldsValue),
    newOrderMultilegCustomFieldsGroupElemCustomFieldsContext :: !(Maybe CustomFieldsContext)
  }
  deriving stock (Show, Eq, Generic)

instance Validity NewOrderMultilegCustomFieldsGroupElem

instance IsComponent NewOrderMultilegCustomFieldsGroupElem where
  toComponentFields ((NewOrderMultilegCustomFieldsGroupElem {..})) =
    mconcat
      [ requiredFieldB newOrderMultilegCustomFieldsGroupElemCustomFieldsName,
        optionalFieldB newOrderMultilegCustomFieldsGroupElemCustomFieldsValue,
        optionalFieldB newOrderMultilegCustomFieldsGroupElemCustomFieldsContext
      ]
  fromComponentFields = do
    newOrderMultilegCustomFieldsGroupElemCustomFieldsName <- requiredFieldP
    newOrderMultilegCustomFieldsGroupElemCustomFieldsValue <- optionalFieldP
    newOrderMultilegCustomFieldsGroupElemCustomFieldsContext <- optionalFieldP
    pure (NewOrderMultilegCustomFieldsGroupElem {..})

instance IsGroupElement NewOrderMultilegCustomFieldsGroupElem where
  type GroupNumField NewOrderMultilegCustomFieldsGroupElem = NoCustomFields
  mkGroupNum Proxy = NoCustomFields
  countGroupNum Proxy = unNoCustomFields

makeNewOrderMultilegCustomFieldsGroupElem :: CustomFieldsName -> NewOrderMultilegCustomFieldsGroupElem
makeNewOrderMultilegCustomFieldsGroupElem newOrderMultilegCustomFieldsGroupElemCustomFieldsName =
  let newOrderMultilegCustomFieldsGroupElemCustomFieldsValue = Nothing
      newOrderMultilegCustomFieldsGroupElemCustomFieldsContext = Nothing
   in (NewOrderMultilegCustomFieldsGroupElem {..})