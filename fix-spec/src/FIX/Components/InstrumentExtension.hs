{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Components.InstrumentExtension where

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Validity
import FIX.Components.Class
import FIX.Fields.DeliveryForm
import FIX.Fields.MsgType
import FIX.Fields.PctAtRisk
import FIX.Groups.Class
import FIX.Groups.InstrAttribGroupElem
import GHC.Generics (Generic)

-- | ComponentSpec
--   { componentName = "InstrumentExtension"
--   , componentPieces =
--       [ MessagePieceField "DeliveryForm" False
--       , MessagePieceField "PctAtRisk" False
--       , MessagePieceGroup
--           GroupSpec
--             { groupName = "InstrAttrib"
--             , groupNumberField = "NoInstrAttrib"
--             , groupPieces =
--                 [ MessagePieceField "InstrAttribType" True
--                 , MessagePieceField "InstrAttribValue" False
--                 ]
--             }
--           False
--       ]
--   }
data InstrumentExtension = InstrumentExtension
  { instrumentExtensionDeliveryForm :: !(Maybe DeliveryForm),
    instrumentExtensionPctAtRisk :: !(Maybe PctAtRisk),
    instrumentExtensionInstrAttribGroup :: ![InstrAttribGroupElem]
  }
  deriving stock (Show, Eq, Generic)

instance Validity InstrumentExtension

instance IsComponent InstrumentExtension where
  toComponentFields ((InstrumentExtension {..})) =
    mconcat
      [ optionalFieldB instrumentExtensionDeliveryForm,
        optionalFieldB instrumentExtensionPctAtRisk,
        optionalGroupB instrumentExtensionInstrAttribGroup
      ]
  fromComponentFields = do
    instrumentExtensionDeliveryForm <- optionalFieldP
    instrumentExtensionPctAtRisk <- optionalFieldP
    instrumentExtensionInstrAttribGroup <- optionalGroupP
    pure (InstrumentExtension {..})

makeInstrumentExtension :: InstrumentExtension
makeInstrumentExtension =
  let instrumentExtensionDeliveryForm = Nothing
      instrumentExtensionPctAtRisk = Nothing
      instrumentExtensionInstrAttribGroup = []
   in (InstrumentExtension {..})