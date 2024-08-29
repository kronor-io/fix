{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module FIX.Groups.Class where

import Control.Monad
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import FIX.Components.Class
import FIX.Core

class (IsField (GroupNumField a), IsComponent a) => IsGroup a where
  type GroupNumField a :: Type
  mkGroupNum :: Proxy a -> Word -> GroupNumField a
  countGroupNum :: Proxy a -> GroupNumField a -> Word

-- TODO use this when parsing:
-- https://www.fixtrading.org/standards/tagvalue-online/
-- @
-- Within a repeating group, field sequence is strictly defined by a group definition.
-- @
requiredGroupB :: (IsGroup a) => NonEmpty a -> [Field]
requiredGroupB = concatMap toComponentFields

optionalGroupB :: (IsGroup a) => [a] -> [Field]
optionalGroupB = concatMap toComponentFields

-- @
-- The NumInGroup field is required and must be larger than zero if the repeating group is required,
-- @
requiredGroupP :: forall a. (IsGroup a) => ComponentP (NonEmpty a)
requiredGroupP = do
  gn <- requiredFieldP
  let count = countGroupNum (Proxy :: Proxy a) gn
  elems <- replicateM (fromIntegral count) requiredComponentP
  case NE.nonEmpty elems of
    Nothing -> error "TODO typed error"
    Just ne -> pure ne

optionalGroupP :: forall a. (IsGroup a) => ComponentP [a]
optionalGroupP = do
  gn <- optionalFieldP
  let count = maybe 0 (countGroupNum (Proxy :: Proxy a)) gn
  replicateM (fromIntegral count) requiredComponentP
