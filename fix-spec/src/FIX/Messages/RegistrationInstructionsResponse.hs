{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.RegistrationInstructionsResponse where

import Data.Validity
import FIX.Fields.Account
import FIX.Fields.AcctIDSource
import FIX.Fields.ClOrdID
import FIX.Fields.RegistID
import FIX.Fields.RegistRefID
import FIX.Fields.RegistRejReasonCode
import FIX.Fields.RegistRejReasonText
import FIX.Fields.RegistStatus
import FIX.Fields.RegistTransType
import GHC.Generics (Generic)

-- MessageSpec {messageName = "RegistrationInstructionsResponse", messageType = "p", messageCategory = "app", messagePieces = [MessagePieceField "RegistID" True,MessagePieceField "RegistTransType" True,MessagePieceField "RegistRefID" True,MessagePieceField "ClOrdID" False,MessagePieceComponent "Parties" False,MessagePieceField "Account" False,MessagePieceField "AcctIDSource" False,MessagePieceField "RegistStatus" True,MessagePieceField "RegistRejReasonCode" False,MessagePieceField "RegistRejReasonText" False]}
data RegistrationInstructionsResponse = RegistrationInstructionsResponse
  { registrationInstructionsResponseRegistID :: !RegistID,
    registrationInstructionsResponseRegistTransType :: !RegistTransType,
    registrationInstructionsResponseRegistRefID :: !RegistRefID,
    registrationInstructionsResponseClOrdID :: !(Maybe ClOrdID),
    registrationInstructionsResponseAccount :: !(Maybe Account),
    registrationInstructionsResponseAcctIDSource :: !(Maybe AcctIDSource),
    registrationInstructionsResponseRegistStatus :: !RegistStatus,
    registrationInstructionsResponseRegistRejReasonCode :: !(Maybe RegistRejReasonCode),
    registrationInstructionsResponseRegistRejReasonText :: !(Maybe RegistRejReasonText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity RegistrationInstructionsResponse
