{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module FIX.Groups.Class where

import Control.Monad
import Control.Monad.Reader
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import FIX.Components.Class
import FIX.Fields

class (IsAnyField (GroupNumField a), IsComponent a) => IsGroupElement a where
  type GroupNumField a :: Type
  mkGroupNum :: Proxy a -> Word -> GroupNumField a
  countGroupNum :: Proxy a -> GroupNumField a -> Word

-- TODO use this when parsing:
-- https://www.fixtrading.org/standards/tagvalue-online/
-- @
-- Within a repeating group, field sequence is strictly defined by a group definition.
-- @
requiredGroupB :: forall a. (IsGroupElement a) => NonEmpty a -> DList AnyField
requiredGroupB ne = do
  DList.cons
    (packAnyField (mkGroupNum (Proxy :: Proxy a) (fromIntegral (length ne))))
    (foldMap toComponentFields ne)

optionalGroupB :: (IsGroupElement a) => [a] -> DList AnyField
optionalGroupB es = case NE.nonEmpty es of
  Nothing -> mempty
  Just ne -> requiredGroupB ne

-- @
-- The NumInGroup field is required and must be larger than zero if the repeating group is required,
-- @
requiredGroupP :: forall a. (IsGroupElement a) => ComponentP (NonEmpty a)
requiredGroupP = do
  gn <- requiredFieldP
  local (const True) $ do
    let count = countGroupNum (Proxy :: Proxy a) gn
    elems <- replicateM (fromIntegral count) requiredComponentP
    case NE.nonEmpty elems of
      Nothing -> error "TODO typed error"
      Just ne -> pure ne

optionalGroupP :: forall a. (IsGroupElement a) => ComponentP [a]
optionalGroupP = do
  gn <- optionalFieldP
  local (const True) $ do
    let count = maybe 0 (countGroupNum (Proxy :: Proxy a)) gn
    replicateM (fromIntegral count) requiredComponentP
