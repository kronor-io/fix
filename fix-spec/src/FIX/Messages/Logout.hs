{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Logout where

import Data.Validity
import FIX.Fields.EncodedText
import FIX.Fields.EncodedTextLen
import FIX.Fields.Text
import GHC.Generics (Generic)

-- MessageSpec {messageName = "Logout", messageType = "5", messageCategory = "admin", messagePieces = [MessagePieceField "Text" False,MessagePieceField "EncodedTextLen" False,MessagePieceField "EncodedText" False]}
data Logout = Logout
  { logoutText :: !(Maybe Text),
    logoutEncodedTextLen :: !(Maybe EncodedTextLen),
    logoutEncodedText :: !(Maybe EncodedText)
  }
  deriving stock (Show, Eq, Generic)

instance Validity Logout
