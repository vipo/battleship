{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Interface (
  Moves(..), MoveResult(..), GameVariation(..)
) where

import Data.Aeson.Types
import Data.Scientific
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

data GameVariation = Classic | Tetris | TShape

data MoveResult = Miss | Hit
  deriving (Eq, Show, Gen.Generic)
instance ToJSON MoveResult where
  toJSON Miss = String "MISS"
  toJSON Hit = String "HIT"

data Moves = Moves {
  coord :: [String]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
} deriving Gen.Generic
instance ToJSON Moves


