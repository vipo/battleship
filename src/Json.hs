{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import qualified Data.Aeson as A
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as BSL

import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Interface as I

instance ToJSON MoveResult where
  toJSON Miss = String "MISS"
  toJSON Hit = String "HIT"

instance FromJSON MoveResult where
  parseJSON (String "MISS") = return Miss
  parseJSON (String "HIT") = return Hit
  parseJSON invalid = typeMismatch "MoveResult" invalid

instance ToJSON Moves
instance FromJSON Moves

instance JsonLike Value where
  toJsonLike (Object m) = JLMap $ map (\(k, v) -> (k, toJsonLike v)) (HMS.toList m)
  toJsonLike (Array v) = JLArray $ map toJsonLike $ V.toList v
  toJsonLike a = JLRaw a

  fromJsonLike (JLMap m) = Object $ HMS.fromList $ map (\(k, v) -> (k, fromJsonLike v)) m
  fromJsonLike (JLArray v) = Array $ V.fromList $ map fromJsonLike v
  fromJsonLike (JLRaw a) = a

  stringKey = JLRaw . String

jsonDecode :: FromJSON a => BSL.ByteString -> Either String a
jsonDecode a = case A.decode a of
  Just v -> Right v
  Nothing -> Left "Invalid json"

fromJsonValue :: FromJSON a => Value -> Either String a
fromJsonValue a = case A.fromJSON a of
  Error s -> Left s
  Success a -> Right a

withOutLists :: Value -> Value
withOutLists = fromJsonLike . I.withOutLists . toJsonLike

fromWithOutLists :: FromJSON a => Value -> Either String a
fromWithOutLists v = do
  transformed <- I.fromWithOutLists (toJsonLike v)
  let js = fromJsonLike transformed
  fromJsonValue js

withOutMaps :: Value -> Value
withOutMaps = fromJsonLike . I.withOutMaps . toJsonLike