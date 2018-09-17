{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ExistentialQuantification #-}

module Domain (
  arbitraryGame, toNestedMoves, Column(..), Row(..), Move(..), Game(..)
) where

import Boards

import qualified Interface as I
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import Data.String.Conversions
import Data.Maybe

import System.Random (RandomGen, mkStdGen, getStdGen, split, next)
import System.Random.Shuffle(shuffle')

data Game = Game {
    firstAttack :: Coordinates
  , replies :: [Move]
} deriving Show

data Move =
    ReplyAndAttack Coordinates I.MoveResult
  | LastReply I.MoveResult
  deriving (Gen.Generic, Show)

type Seed = Int

data CS = forall a . CoordinatesSet a => CS a

arbitraryGame :: I.GameVariation -> Maybe Seed -> IO I.Moves
arbitraryGame game seed = do
  gen <- maybe getStdGen (return . mkStdGen) seed
  let limit = ((`mod` 300) . abs . fst . next) gen
  let (gen1, gen2) = split gen
  let css = (boardFor game gen1, boardFor game gen2)
  let moves = (randomMoves gen1, randomMoves gen2)
  return $ toNestedMoves $ playAGame limit css moves
  where
    boardFor :: RandomGen g => I.GameVariation -> g -> CS
    boardFor I.Classical = CS . classicalGameBoard
    boardFor I.Tetris = CS . tetrisGameBoard
    boardFor I.TShape = CS . tshapesGameBoard
    allCoords = [(col, row) | col <- allCols, row <- allRows]
    randomMoves :: RandomGen g => g -> [Coordinates]
    randomMoves = shuffle' allCoords (length allCoords)

playAGame :: Int -> (CS, CS) -> ([Coordinates], [Coordinates]) -> Game
playAGame _ _ ([],_) = error "Empty game"
playAGame limit (cs1, cs2) (initial:moves1, moves2) =
  react limit initial (moves2, cs2) (moves1, cs1) (Game initial [])
  where
    react :: Int -> Coordinates -> ([Coordinates], CS) -> ([Coordinates], CS) -> Game -> Game
    react 0 _ _ _ acc = acc
    react _ c ([], b) _ (Game f a) =      Game f (LastReply (result c b):a)
    react _ _ _ ([], _) _ = error "Illegal state"
    react l c mine@(_, b) ([m], b2) (Game f a) = react (l-1) m ([],b2) mine $ Game f (ReplyAndAttack m (result c b):a)
    react l c mine@(_, b) (m:their, b2) (Game f a) = react (l-1) m (their,b2) mine $ Game f (ReplyAndAttack m (result c b):a)
    result :: Coordinates -> CS -> I.MoveResult
    result c (CS b) = if b `contains` c then I.Hit else I.Miss

toNestedMoves :: Game -> I.Moves
toNestedMoves (Game c r) = toNestedMoves' (reverse r) (I.Moves (mapCoord c) Nothing Nothing)
  where
    toNestedMoves' :: [Move] -> I.Moves -> I.Moves
    toNestedMoves' [] acc = acc
    toNestedMoves' (ReplyAndAttack cor res : t) acc = toNestedMoves' t (I.Moves (mapCoord cor) (Just res) (Just acc))
    toNestedMoves' (LastReply res : _) acc = I.Moves [] (Just res) (Just acc)

    mapCoord :: (Column, Row) -> [T.Text]
    mapCoord (c, r) = [cs (show c), cs (show r)]
