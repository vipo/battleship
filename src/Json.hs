{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Data.Aeson.Types

import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Interface as I

instance ToJSON MoveResult where
  toJSON Miss = String "MISS"
  toJSON Hit = String "HIT"

instance ToJSON Moves

instance JsonLike Value where
  toJsonLike (Object m) = JLMap $ map (\(k, v) -> (k, toJsonLike v)) (HMS.toList m)
  toJsonLike (Array v) = JLArray $ map toJsonLike $ V.toList v
  toJsonLike a = JLRaw a

  fromJsonLike (JLMap m) = Object $ HMS.fromList $ map (\(k, v) -> (k, fromJsonLike v)) m
  fromJsonLike (JLArray v) = Array $ V.fromList $ map fromJsonLike v
  fromJsonLike (JLRaw a) = a

  stringKey = JLRaw . String

withOutLists ::Value -> Value
withOutLists = fromJsonLike . I.withOutLists . toJsonLike

withOutMaps :: Value -> Value
withOutMaps = fromJsonLike . I.withOutMaps . toJsonLike