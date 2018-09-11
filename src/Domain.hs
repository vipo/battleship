{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Domain (
  arbitraryGame, toNestedMoves, Column(..), Row(..), Move(..), Game(..)
) where

import Boards

import qualified Interface as I
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import Data.String.Conversions

data Game = Game {
    firstAttack :: Coordinates
  , replies :: [Move]
}

data Move =
    ReplyAndAttack Coordinates I.MoveResult
  | LastReply I.MoveResult
  deriving Gen.Generic

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
