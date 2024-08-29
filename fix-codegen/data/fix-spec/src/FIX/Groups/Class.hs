{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module FIX.Groups.Class where

import Data.List.NonEmpty (NonEmpty)
import FIX.Components.Class
import FIX.Core

class (IsField (GroupNumField a), IsComponent a) => IsGroup a where
  type GroupNumField a

requiredGroupB :: (IsGroup a) => NonEmpty a -> [Field]
requiredGroupB = concatMap toComponentFields

optionalGroupB :: (IsGroup a) => Maybe (NonEmpty a) -> [Field]
optionalGroupB = maybe [] requiredGroupB

requiredGroupP :: ComponentP a
requiredGroupP = undefined

optionalGroupP :: ComponentP a
optionalGroupP = undefined
