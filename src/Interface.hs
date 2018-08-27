{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Interface (
  JsonLike(..), JsonLikeValue(..),
  Moves(..), MoveResult(..), GameVariation(..), 
  withOutLists, withOutMaps
) where

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Data.Text
import Text.Printf
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import Type.Reflection

data GameVariation = Classical | Tetris | TShape
  deriving (Gen.Generic)

data MoveResult = Miss | Hit
  deriving (Eq, Show, Gen.Generic, Typeable)

data Moves = Moves {
  coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
} deriving (Gen.Generic, Show, Eq, Typeable)

data JsonLikeValue a = JLMap [(Text, JsonLikeValue a)] | JLArray [JsonLikeValue a] | JLRaw a

class JsonLike a where
  toJsonLike :: a -> JsonLikeValue a
  fromJsonLike :: JsonLikeValue a -> a 
  stringKey :: Text -> JsonLikeValue a

lexOrdered :: Int -> [T.Text]
lexOrdered a = L.sort $ L.map TS.showt [1..a]

withOutLists :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutLists (JLMap m) = JLMap $ L.map (\(k, v) -> (k, withOutLists v)) m
withOutLists (JLArray v) = JLMap $ L.zip (lexOrdered (L.length v)) (L.map withOutLists v)
withOutLists a = a

withOutMaps :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutMaps (JLMap m) = JLArray $ L.concatMap (\(k, v) -> [stringKey k, withOutMaps v]) m
withOutMaps (JLArray v) = JLArray $ L.map withOutMaps v
withOutMaps a = a