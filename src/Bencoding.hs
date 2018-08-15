{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Bencoding where

import Data.BEncode
import qualified Data.BEncode.BDict as BDict

import qualified Data.List as L

import Interface as I

import Data.String.Conversions

instance BEncode MoveResult where
  toBEncode Miss = BString "MISS"
  toBEncode Hit = BString "HIT"
  fromBEncode _ = Left "Not Implemented"

instance BEncode Moves where
  toBEncode Moves {..} = toDict $
      "coord" .=! map toBEncode coord .:
      "prev" .=? prev .:
      "result" .=? result .:
      endDict
  fromBEncode _ = Left "Not Implemented"
  
instance JsonLike BValue where
  toJsonLike (BDict m) = JLMap $ map (\(k, v) -> (cs k, toJsonLike v)) (BDict.toAscList m)
  toJsonLike (BList v) = JLArray $ map toJsonLike v
  toJsonLike a = JLRaw a

  fromJsonLike (JLMap m) = BDict $ BDict.fromAscList $ L.map (\(k, v) -> (cs k, fromJsonLike v)) m
  fromJsonLike (JLArray v) = BList $ L.map fromJsonLike v
  fromJsonLike (JLRaw a) = a

  stringKey = JLRaw . BString . cs

withOutLists ::BValue -> BValue
withOutLists = fromJsonLike . I.withOutLists . toJsonLike

withOutMaps :: BValue -> BValue
withOutMaps = fromJsonLike . I.withOutMaps . toJsonLike