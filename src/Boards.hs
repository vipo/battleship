
module Boards where

import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Data.String.Conversions
import qualified Data.List as L

import Data.Maybe
import System.Random
import System.Random.Shuffle(shuffle')
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
  TetrisIShip | TetrisOShip | TetrisLShip | TetrisTShip | TetrisZShip
instance Show TetrisGameShip where
  show TetrisIShip = "I"
  show TetrisOShip = "O"
  show TetrisLShip = "L"
  show TetrisTShip = "T"
  show TetrisZShip = "Z"

data Column = A | B | C | D | E | F | G | H | I | J
  deriving (Show, Eq, Ord, Bounded, Enum, Read)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Eq, Ord, Bounded, Enum)
instance Show Row where
  show r = show (1 + toInteger (fromEnum r))
instance Read Row where
  readsPrec _ s@[c] | c `L.elem` ['1'..'9'] = [(toEnum (read s - 1), "")]
  readsPrec _ "10" = [(R10, "")] 
  readsPrec _ _ = []

type Coordinates = (Column, Row) 

class CoordinatesSet a where
  contains :: a -> Coordinates -> Bool

newtype Board a = BoardMap (Map.Map Coordinates a)

instance CoordinatesSet (Board a) where
  contains (BoardMap m) coords = isJust $ Map.lookup coords m

allCols :: [Column]
allCols = [A .. J]

allRows :: [Row]
allRows = [R1 .. R10]

instance Show a => Show (Board a) where
  show (BoardMap m) = concat $ "  |ABCDEFGHIJ|\n" : dashes : [renderRow row | row <- allRows] ++ [dashes] 
    where
      dashes = "--+----------+\n"
      renderRow :: Row -> String
      renderRow row = concat [
          printf "%2s|" (show row)
        , concat [maybe " " show $ Map.lookup (col, row) m | col <- allCols]
        , "|\n"
        ]

tetrisGameBoard :: RandomGen g => g -> Board TetrisGameShip
tetrisGameBoard gen =
  takeRandom gen $ board1 :| [board2]
  where
    board1 = BoardMap $ Map.fromList [
        ((B,R2), TetrisOShip), ((B,R3), TetrisOShip), ((C,R2), TetrisOShip), ((C,R3), TetrisOShip)
      , ((B,R7), TetrisLShip), ((B,R8), TetrisLShip), ((B,R9), TetrisLShip), ((C,R9), TetrisLShip)
      , ((E,R7), TetrisTShip), ((F,R7), TetrisTShip), ((G,R7), TetrisTShip), ((F,R6), TetrisTShip)
      , ((H,R2), TetrisZShip), ((H,R3), TetrisZShip), ((G,R3), TetrisZShip), ((G,R4), TetrisZShip)
      , ((I,R6), TetrisIShip), ((I,R7), TetrisIShip), ((I,R8), TetrisIShip), ((I,R9), TetrisIShip)
      ]
    board2 = BoardMap $ Map.fromList [
        ((I,R9), TetrisOShip), ((I,R10),TetrisOShip), ((J,R9), TetrisOShip), ((J,R10),TetrisOShip)
      , ((I,R1), TetrisLShip), ((J,R1), TetrisLShip), ((J,R2), TetrisLShip), ((J,R3), TetrisLShip)
      , ((A,R10),TetrisTShip), ((B,R10),TetrisTShip), ((C,R10),TetrisTShip), ((B,R9), TetrisTShip)
      , ((F,R7), TetrisZShip), ((G,R7), TetrisZShip), ((G,R6), TetrisZShip), ((H,R6), TetrisZShip)
      , ((A,R2), TetrisIShip), ((A,R3), TetrisIShip), ((A,R4), TetrisIShip), ((A,R5), TetrisIShip)
      ]

tshapesGameBoard :: RandomGen g => g -> Board TShapesGameShip
tshapesGameBoard gen =
  takeRandom gen $ board1 :| [board2]
  where
    board1 = BoardMap $ Map.fromList [
        ((A,R8), TShapeShip), ((B,R8), TShapeShip), ((C,R8), TShapeShip), ((C,R7), TShapeShip), ((C,R9), TShapeShip)
      , ((D,R2), TShapeShip), ((D,R3), TShapeShip), ((D,R4), TShapeShip), ((E,R3), TShapeShip), ((F,R3), TShapeShip)
      , ((H,R2), TShapeShip), ((I,R2), TShapeShip), ((J,R2), TShapeShip), ((I,R3), TShapeShip), ((I,R4), TShapeShip)
      , ((G,R5), TShapeShip), ((G,R6), TShapeShip), ((G,R7), TShapeShip), ((H,R6), TShapeShip), ((I,R6), TShapeShip)
      , ((J,R8), TShapeShip), ((J,R9), TShapeShip), ((J,R10),TShapeShip), ((I,R9), TShapeShip), ((H,R9), TShapeShip)
      ]
    board2 = BoardMap $ Map.fromList [
        ((A,R4), TShapeShip), ((A,R5), TShapeShip), ((A,R6), TShapeShip), ((B,R5), TShapeShip), ((C,R5), TShapeShip)
      , ((B,R1), TShapeShip), ((C,R1), TShapeShip), ((D,R1), TShapeShip), ((C,R2), TShapeShip), ((C,R3), TShapeShip)
      , ((J,R2), TShapeShip), ((J,R3), TShapeShip), ((J,R4), TShapeShip), ((I,R3), TShapeShip), ((H,R3), TShapeShip)
      , ((B,R8), TShapeShip), ((B,R9), TShapeShip), ((B,R10),TShapeShip), ((A,R10),TShapeShip), ((C,R10),TShapeShip)
      , ((F,R8), TShapeShip), ((F,R9), TShapeShip), ((F,R10),TShapeShip), ((E,R10),TShapeShip), ((G,R10),TShapeShip)
      ]

classicalGameBoard :: RandomGen g => g -> Board ClassicalGameShip
classicalGameBoard gen =
  takeRandom gen $ board1 :| [board2]
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

takeRandom :: RandomGen g => g -> NE.NonEmpty a -> a
takeRandom gen list =
  head $ shuffle' asList (length asList) gen
  where
    asList = NE.toList list