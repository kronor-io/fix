{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Components.Class where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State
import Data.Proxy
import FIX.Core
import FIX.Fields.MsgType

type ComponentP a = StateT [Field] (Except ComponentParseError) a

tryComponentP :: ComponentP a -> ComponentP (Maybe a)
tryComponentP = undefined

data ComponentParseError
  = -- TODO get this constructor out of here and into a message parse error
    ComponentParseErrorMsgTypeMismatch !MsgType !MsgType
  | ComponentParseErrorMissingField !Tag
  | ComponentParseErrorFieldParseError !Tag !String
  deriving (Show)

class IsComponent a where
  toComponentFields :: a -> [Field]
  fromComponentFields :: ComponentP a

requiredFieldP :: forall a. (IsField a) => ComponentP a
requiredFieldP = do
  let tag = fieldTag (Proxy :: Proxy a)
  mA <- optionalFieldP
  case mA of
    Nothing -> throwError $ ComponentParseErrorMissingField tag
    Just a -> pure a

optionalFieldP :: forall a. (IsField a) => ComponentP (Maybe a)
optionalFieldP = do
  fields <- get
  let tag = fieldTag (Proxy :: Proxy a)
  let go = \case
        [] -> Nothing
        (f@(Field t v) : fs) ->
          if t == tag
            then pure (v, fs)
            else second (f :) <$> go fs

  case go fields of
    Nothing -> pure Nothing
    Just (b, fields') -> case fieldFromValue (valueByteString b) of
      Left err -> throwError $ ComponentParseErrorFieldParseError tag err
      Right v -> do
        put fields'
        pure (Just v)

fieldB :: forall a. (IsField a) => a -> Field
fieldB a =
  let p = (Proxy :: Proxy a)
   in Field
        (fieldTag p)
        ( ( if fieldIsData p
              then ValueData
              else ValueSimple
          )
            (fieldToValue a)
        )

requiredFieldB :: (IsField a) => a -> [Field]
requiredFieldB = pure . fieldB

optionalFieldB :: (IsField a) => Maybe a -> [Field]
optionalFieldB = maybe [] requiredFieldB

requiredComponentB :: (IsComponent a) => a -> [Field]
requiredComponentB = toComponentFields

optionalComponentB :: (IsComponent a) => Maybe a -> [Field]
optionalComponentB = maybe [] requiredComponentB

requiredComponentP :: (IsComponent a) => ComponentP a
requiredComponentP = fromComponentFields

optionalComponentP :: (IsComponent a) => ComponentP (Maybe a)
optionalComponentP = tryComponentP requiredComponentP
