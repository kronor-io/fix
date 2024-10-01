{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Groups.ExecutionReportTrdRegPublicationsGroupElem where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.MsgType
import FIX.Fields.NoTrdRegPublications
import FIX.Fields.TrdRegPublicationReason
import FIX.Fields.TrdRegPublicationType
import FIX.Groups.Class
import GHC.Generics (Generic)

-- | GroupSpec
--   { groupName = "ExecutionReportTrdRegPublications"
--   , groupNumberField = "NoTrdRegPublications"
--   , groupPieces =
--       [ MessagePieceField "TrdRegPublicationType" True
--       , MessagePieceField "TrdRegPublicationReason" False
--       ]
--   }
data ExecutionReportTrdRegPublicationsGroupElem = ExecutionReportTrdRegPublicationsGroupElem
  { executionReportTrdRegPublicationsGroupElemTrdRegPublicationType :: !TrdRegPublicationType,
    executionReportTrdRegPublicationsGroupElemTrdRegPublicationReason :: !(Maybe TrdRegPublicationReason)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ExecutionReportTrdRegPublicationsGroupElem

instance IsComponent ExecutionReportTrdRegPublicationsGroupElem where
  toComponentFields ((ExecutionReportTrdRegPublicationsGroupElem {..})) =
    mconcat
      [ requiredFieldB executionReportTrdRegPublicationsGroupElemTrdRegPublicationType,
        optionalFieldB executionReportTrdRegPublicationsGroupElemTrdRegPublicationReason
      ]
  fromComponentFields = do
    executionReportTrdRegPublicationsGroupElemTrdRegPublicationType <- requiredFieldP
    executionReportTrdRegPublicationsGroupElemTrdRegPublicationReason <- optionalFieldP
    pure (ExecutionReportTrdRegPublicationsGroupElem {..})

instance IsGroupElement ExecutionReportTrdRegPublicationsGroupElem where
  type GroupNumField ExecutionReportTrdRegPublicationsGroupElem = NoTrdRegPublications
  mkGroupNum Proxy = NoTrdRegPublications
  countGroupNum Proxy = unNoTrdRegPublications

makeExecutionReportTrdRegPublicationsGroupElem :: TrdRegPublicationType -> ExecutionReportTrdRegPublicationsGroupElem
makeExecutionReportTrdRegPublicationsGroupElem executionReportTrdRegPublicationsGroupElemTrdRegPublicationType =
  let executionReportTrdRegPublicationsGroupElemTrdRegPublicationReason = Nothing
   in (ExecutionReportTrdRegPublicationsGroupElem {..})