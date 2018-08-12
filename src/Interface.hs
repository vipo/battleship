{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Interface (
  Moves(..), MoveResult(..), GameVariation(..), 
  withOutLists, withOutMaps
) where

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Data.Text
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

data GameVariation = Classic | Tetris | TShape

data MoveResult = Miss | Hit
  deriving (Eq, Show, Gen.Generic)

data Moves = Moves {
  coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
} deriving (Gen.Generic, Show, Eq)

withOutLists :: A.Value -> A.Value
withOutLists (A.Object m) = A.Object $ HMS.map withOutLists m
withOutLists (A.Array v) = A.Object $ HMS.fromList $ L.zip (L.map TS.showt ([1 .. ] :: [Integer])) (L.map withOutLists (V.toList v))
withOutLists a = a

withOutMaps :: A.Value -> A.Value
withOutMaps (A.Object m) = A.Array $ V.fromList $ L.concatMap (\(k, v) -> [A.String k, withOutMaps v]) (HMS.toList m)
withOutMaps (A.Array v) = A.Array $ V.map withOutMaps v
withOutMaps a = a