{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Components.Class where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.DList (DList)
import Data.Proxy
import FIX.Core
import FIX.Fields

-- The bool indicates whether fields must be parsed in order (in a group)
type ComponentP a = ReaderT Bool (StateT [AnyField] (Except ComponentParseError)) a

runComponentP :: [AnyField] -> ComponentP a -> Either ComponentParseError a
runComponentP fields func = runExcept $ evalStateT (runReaderT func False) fields

tryComponentP :: ComponentP a -> ComponentP (Maybe a)
tryComponentP func = do
  inOrder <- ask
  fields <- get
  case runExcept $ runStateT (runReaderT func inOrder) fields of
    Left _ -> pure Nothing
    Right (c, fields') -> do
      put fields'
      pure (Just c)

data ComponentParseError
  = -- TODO get this constructor out of here and into a message parse error
    ComponentParseErrorMsgTypeMismatch !MsgType !MsgType
  | ComponentParseErrorMissingField !Tag
  | ComponentParseErrorFieldParseError !Tag !String
  deriving (Show)

class IsComponent a where
  toComponentFields :: a -> DList AnyField
  fromComponentFields :: ComponentP a

requiredFieldP :: forall a. (IsAnyField a) => ComponentP a
requiredFieldP = do
  let tag = fieldTag (Proxy :: Proxy a)
  mA <- optionalFieldP
  case mA of
    Nothing -> throwError $ ComponentParseErrorMissingField tag
    Just a -> pure a

optionalFieldP :: forall a. (IsAnyField a) => ComponentP (Maybe a)
optionalFieldP = do
  inOrder <- ask
  fields <- get
  let go = \case
        [] -> Nothing
        (af : fs) ->
          case unpackAnyField af of
            Just f -> pure (f, fs)
            Nothing ->
              if inOrder
                then Nothing
                else second (af :) <$> go fs

  case go fields of
    Nothing -> pure Nothing
    Just (f, fields') -> do
      put fields'
      pure (Just f)

requiredFieldB :: (IsAnyField a) => a -> DList AnyField
requiredFieldB = pure . packAnyField

optionalFieldB :: (IsAnyField a) => Maybe a -> DList AnyField
optionalFieldB = maybe mempty requiredFieldB

requiredComponentB :: (IsComponent a) => a -> DList AnyField
requiredComponentB = toComponentFields

optionalComponentB :: (IsComponent a) => Maybe a -> DList AnyField
optionalComponentB = maybe mempty requiredComponentB

requiredComponentP :: (IsComponent a) => ComponentP a
requiredComponentP = fromComponentFields

optionalComponentP :: (IsComponent a) => ComponentP (Maybe a)
optionalComponentP = tryComponentP requiredComponentP
