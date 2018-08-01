{-# LANGUAGE DeriveGeneric #-}

module Interface (
  NestedMoves(..), Column(..), Row(..), MoveResult(..), GameVariation(..),
) where

import Data.Aeson.Types
import Data.Scientific
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified GHC.Generics as Gen

data GameVariation = Classic | Tetris | TShape

data Column = A | B | C | D | E | F | G | H | J | I
  deriving Gen.Generic
instance ToJSON Column

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Eq, Ord, Bounded, Show, Enum)

instance ToJSON Row where
  toJSON r = Number $ scientific (1 + toInteger (fromEnum r)) 0

data MoveResult = Miss | Hit | Sank
  deriving (Eq, Show, Gen.Generic)
instance ToJSON MoveResult

data NestedMoves = NestedMoves {
    column :: Column
  , row :: Row
  , result :: Maybe MoveResult
  , prev :: Maybe NestedMoves
} deriving Gen.Generic
instance ToJSON NestedMoves
