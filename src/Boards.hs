
module Boards where

import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Data.Maybe
import Text.Printf

import qualified Data.Map.Strict as Map

data ClassicalGameShip =
  LengthFourShip | LengthThreeShip | LengthTwoShip | LengthOneShip
instance Show ClassicalGameShip where
  show LengthFourShip  = "4"
  show LengthThreeShip = "3"
  show LengthTwoShip   = "2"
  show LengthOneShip   = "1"

data TShapesGameShip = TShapeShip
instance Show TShapesGameShip where
  show _ = "T"

data TetrisGameShip =
  TetrisIShape | TetrisOShape | TetrisLShape | TetrisTShape | TetrisZShape
instance Show TetrisGameShip where
  show TetrisIShape = "I"
  show TetrisOShape = "O"
  show TetrisLShape = "L"
  show TetrisTShape = "T"
  show TetrisZShape = "Z"

data Column = A | B | C | D | E | F | G | H | I | J
  deriving (Show, Eq, Ord, Bounded, Enum)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Eq, Ord, Bounded, Enum)
instance Show Row where
  show r = show (1 + toInteger (fromEnum r))

type Coordinates = (Column, Row) 

newtype Board a = BoardMap (Map.Map Coordinates a)

instance Show a => Show (Board a) where
  show (BoardMap m) = concat $ "  |ABCDEFGHIJ|\n" : dashes : [renderRow row | row <- [R1 .. R10]] ++ [dashes] 
    where
      dashes = "--+----------+\n"
      renderRow :: Row -> String
      renderRow row = concat [
          printf "%2s|" (show row)
        , concat [maybe " " show $ Map.lookup (col, row) m | col <- [A .. J]]
        , "|\n"
        ]

type Seed = Int

classicalGameBoard :: Seed -> Board ClassicalGameShip
classicalGameBoard seed =
  takeRandom seed $ board1 :| [board2]
  where
    board1 = BoardMap $ Map.fromList [
        ((B,R8), LengthFourShip),  ((C,R8),LengthFourShip),  ((D,R8),LengthFourShip), ((E,R8),LengthFourShip)
      , ((H,R9), LengthThreeShip), ((I,R9),LengthThreeShip), ((J,R9),LengthThreeShip)
      , ((H,R2), LengthThreeShip), ((H,R3),LengthThreeShip), ((H,R4),LengthThreeShip)
      , ((E,R1), LengthTwoShip),   ((F,R1),LengthTwoShip)
      , ((A,R2), LengthTwoShip),   ((A,R3),LengthTwoShip)
      , ((B,R5), LengthTwoShip),   ((B,R6),LengthTwoShip)
      , ((D,R3), LengthOneShip)
      , ((B,R10),LengthOneShip)
      , ((F,R10),LengthOneShip)
      , ((I,R6), LengthOneShip)
      ]
    board2 = BoardMap $ Map.fromList [
        ((J,R1), LengthFourShip),  ((J,R2), LengthFourShip),  ((J,R3), LengthFourShip), ((J,R4),LengthFourShip)
      , ((J,R6), LengthThreeShip), ((J,R7), LengthThreeShip), ((J,R8), LengthThreeShip)
      , ((A,R10),LengthThreeShip), ((B,R10),LengthThreeShip), ((C,R10),LengthThreeShip)
      , ((A,R1), LengthTwoShip),   ((B,R1), LengthTwoShip)
      , ((A,R6), LengthTwoShip),   ((A,R7), LengthTwoShip)
      , ((I,R10),LengthTwoShip),   ((J,R10),LengthTwoShip)
      , ((B,R4), LengthOneShip)
      , ((G,R3), LengthOneShip)
      , ((E,R6), LengthOneShip)
      , ((F,R8), LengthOneShip)
      ]

takeRandom :: Seed -> NE.NonEmpty a -> a
takeRandom seed list =
  head $ NE.drop (seed `mod` NE.length list) list