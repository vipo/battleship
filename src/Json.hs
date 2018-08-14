{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Json where

import Data.Aeson.Types

import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Interface

instance ToJSON MoveResult where
  toJSON Miss = String "MISS"
  toJSON Hit = String "HIT"

instance ToJSON Moves

instance JsonLike Value where
  toJsonLike (Object m) = JLMap $ map (\(k, v) -> (k, toJsonLike v)) (HMS.toList m)
  toJsonLike (Array v) = JLArray $ map toJsonLike $ V.toList v
  toJsonLike a = JLRaw a
