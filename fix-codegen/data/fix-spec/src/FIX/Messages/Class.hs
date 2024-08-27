{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FIX.Messages.Class where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State
import Data.Proxy
import FIX.Core
import FIX.Fields.MsgType

type MessageP a = StateT [Field] (Except MessageParseError) a

data MessageParseError
  = MessageParseErrorMessageTypeMismatch !MsgType !MsgType
  | MessageParseErrorMissingField !Tag
  | MessageParseErrorFieldParseError !Tag !String
  deriving (Show)

class IsMessage a where
  messageType :: Proxy a -> MsgType
  toMessageFields :: a -> [Field]
  fromMessageFields :: MessageP a

requiredFieldP :: forall a. (IsField a) => MessageP a
requiredFieldP = do
  let tag = fieldTag (Proxy :: Proxy a)
  mA <- optionalFieldP
  case mA of
    Nothing -> throwError $ MessageParseErrorMissingField tag
    Just a -> pure a

optionalFieldP :: forall a. (IsField a) => MessageP (Maybe a)
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
      Left err -> throwError $ MessageParseErrorFieldParseError tag err
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

requiredFieldB :: (IsField a) => a -> Maybe Field
requiredFieldB = Just . fieldB

optionalFieldB :: (IsField a) => Maybe a -> Maybe Field
optionalFieldB = (>>= requiredFieldB)
