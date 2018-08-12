{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Domain (
  arbitraryGame, toNestedMoves, Column(..), Row(..), Move(..), Game(..)
) where

import qualified Interface as I
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import Data.String.Conversions

data Column = A | B | C | D | E | F | G | H | J | I
  deriving Show

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Eq, Ord, Bounded, Enum)
instance Show Row where
  show r = show (1 + toInteger (fromEnum r))

type Coordinates = (Column, Row) 

data Game = Game {
    firstAttack :: Coordinates
  , replies :: [Move]
}

data Move =
    ReplyAndAttack Coordinates I.MoveResult
  | LastReply I.MoveResult
  deriving Gen.Generic

type Seed = Integer

arbitraryGame :: I.GameVariation -> Maybe Seed -> IO I.Moves
arbitraryGame _ _ = return $ toNestedMoves some
  where
    some = Game (A, R1) [ReplyAndAttack (C, R3) I.Miss, ReplyAndAttack (B, R2) I.Miss]
    
toNestedMoves :: Game -> I.Moves
toNestedMoves (Game c r) = toNestedMoves' (reverse r) (I.Moves (mapCoord c) Nothing Nothing)
  where
    toNestedMoves' :: [Move] -> I.Moves -> I.Moves
    toNestedMoves' [] acc = acc
    toNestedMoves' (ReplyAndAttack c res : t) acc = toNestedMoves' t (I.Moves (mapCoord c) (Just res) (Just acc))
    toNestedMoves' (LastReply res : _) acc = I.Moves [] (Just res) (Just acc)

    mapCoord :: (Column, Row) -> [T.Text]
    mapCoord (c, r) = [cs (show c), cs (show r)]
