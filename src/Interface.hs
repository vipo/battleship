{-# LANGUAGE DeriveGeneric #-}

module Interface (
  NestedMoves(..), MoveResult(..), GameVariation(..)
) where

import Data.Aeson.Types
import Data.Scientific
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

data GameVariation = Classic | Tetris | TShape

data MoveResult = Miss | Hit | Sank
  deriving (Eq, Show, Gen.Generic)
instance ToJSON MoveResult

data NestedMoves = NestedMoves {
  coord :: [String]
  , result :: Maybe MoveResult
  , prev :: Maybe NestedMoves
} deriving Gen.Generic
instance ToJSON NestedMoves
