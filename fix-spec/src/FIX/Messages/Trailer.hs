{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module has been GENERATED by fix-codegen.
-- Any manual edits will be undone the next time fix-codegen is run.
module FIX.Messages.Trailer where

import Data.Validity
import FIX.Components.Class
import FIX.Fields.CheckSum
import GHC.Generics (Generic)

data Trailer = Trailer {trailerCheckSum :: !CheckSum}
  deriving stock (Show, Eq, Generic)

instance Validity Trailer

instance IsComponent Trailer where
  toComponentFields ((Trailer {..})) = mconcat [requiredFieldB trailerCheckSum]
  fromComponentFields = do
    trailerCheckSum <- requiredFieldP
    pure (Trailer {..})

makeTrailer :: CheckSum -> Trailer
makeTrailer trailerCheckSum =
  let
   in (Trailer {..})
