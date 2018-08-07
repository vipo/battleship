{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain (
  arbitraryGame
) where

import qualified Interface as I
import qualified GHC.Generics as Gen

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

toNestedMoves :: Game -> I.NestedMoves
toNestedMoves (Game c r) = toNestedMoves' (reverse r) (I.NestedMoves (mapCoord c) Nothing Nothing)

toNestedMoves' :: [Move] -> I.NestedMoves -> I.NestedMoves
toNestedMoves' [] acc = acc
toNestedMoves' (ReplyAndAttack c res : t) acc = toNestedMoves' t (I.NestedMoves (mapCoord c) (Just res) (Just acc))
toNestedMoves' (LastReply res : _) acc = I.NestedMoves [] (Just res) (Just acc)

mapCoord :: (Column, Row) -> [String]
mapCoord (c, r) = [show c, show r]

type Seed = Integer

arbitraryGame :: I.GameVariation -> Maybe Seed -> IO I.NestedMoves
arbitraryGame _ _ = return $ toNestedMoves some
  where
    some = Game (A, R1) [ReplyAndAttack (C, R3) I.Miss, ReplyAndAttack (B, R2) I.Miss]