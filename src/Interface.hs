{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Interface (
  Moves(..), MoveResult(..), GameVariation(..)
) where

import Data.Text
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

data GameVariation = Classic | Tetris | TShape

data MoveResult = Miss | Hit
  deriving (Eq, Show, Gen.Generic)

data Moves = Moves {
  coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
} deriving Gen.Generic
