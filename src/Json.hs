{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Control.Monad ((>=>))

import qualified Data.Aeson as A
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as BSL

import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Interface as I

instance JsonLike Value where
  toJsonLike (Object m) = JLMap $ map (\(k, v) -> (k, toJsonLike v)) (HMS.toList m)
  toJsonLike (Array v) = JLArray $ map toJsonLike $ V.toList v
  toJsonLike Null = JLNull
  toJsonLike (String t) = JLString t
  toJsonLike a = JLRaw a

  fromJsonLike (JLMap m) = Object $ HMS.fromList $ map (\(k, v) -> (k, fromJsonLike v)) m
  fromJsonLike (JLArray v) = Array $ V.fromList $ map fromJsonLike v
  fromJsonLike JLNull = Null
  fromJsonLike (JLString t) = String t
  fromJsonLike (JLRaw a) = a

  stringKey = JLRaw . String

jsonDecode :: BSL.ByteString -> Either String Value
jsonDecode a = case A.decode a of
  Just v -> Right v
  Nothing -> Left "Invalid json"

fromJsonValue :: FromJSON a => Value -> Either String a
fromJsonValue a = case A.fromJSON a of
  Error s -> Left s
  Success a -> Right a

withOutLists :: Value -> Value
withOutLists = fromJsonLike . I.withOutLists . toJsonLike

transform :: FromJSON a
  => (JsonLikeValue Value -> Either String Value)
  -> BSL.ByteString
  -> Either String a
transform transformer a = do
  v <- jsonDecode a
  transformed <- transformer (toJsonLike v)
  fromJsonValue transformed

fromWithOutLists :: FromJSON a => BSL.ByteString -> Either String a
fromWithOutLists = transform (I.fromWithOutLists >=> return . fromJsonLike)

fromWithOutMaps :: FromJSON a => BSL.ByteString -> Either String a
fromWithOutMaps = transform (I.fromWithOutMaps >=> return . fromJsonLike)

withOutMaps :: Value -> Value
withOutMaps = fromJsonLike . I.withOutMaps . toJsonLike