{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

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
  return $ toNestedMoves $ playAGame limit css moves (hitsPerGame game)
  where
    boardFor :: RandomGen g => I.GameVariation -> g -> CS
    boardFor I.Classical = CS . classicalGameBoard
    boardFor I.Tetris = CS . tetrisGameBoard
    boardFor I.TShape = CS . tshapesGameBoard
    allCoords = [(col, row) | col <- allCols, row <- allRows]
    randomMoves :: RandomGen g => g -> [Coordinates]
    randomMoves = shuffle' allCoords (length allCoords)

type Hits = (Int, Int)
type HitDecreaser = Hits -> Hits
type MoveState = (CS, Coordinates, HitDecreaser)

playAGame :: Int -> (CS, CS) -> ([Coordinates], [Coordinates]) -> Int -> Game
playAGame limit (cs1, cs2) (moves1, moves2) maxHits = start moves
  where
    moves :: [MoveState]
    moves = zip (map (cs1,,\(a,b) -> (a-1,b)) moves1) (map (cs2,,\(a,b) -> (a,b-1)) moves2) >>= (\t -> [fst t, snd t])
    start :: [MoveState] -> Game
    start [] = error "Empty game"
    start ((b,c,d):t) = react (limit, (maxHits, maxHits)) (result c b, d) t (Game c [])
    react :: (Int, (Int, Int)) -> (I.MoveResult, HitDecreaser) -> [MoveState] -> Game -> Game
    react (0,_) _ _ acc = acc
    react _ (r,_) [] (Game f a) = Game f (LastReply r : a)
    react (l,h) (r,d) ((b,c,nd):t) (Game f a) =
      case decHits r h d of
        (h1,h2) | h1 <= 0 || h2 <= 0 -> Game f (LastReply r : a)
        nh -> react (l-1, nh) (result c b, nd) t (Game f (ReplyAndAttack c r : a))
    result :: Coordinates -> CS -> I.MoveResult
    result c (CS b) = if b `contains` c then I.Hit else I.Miss
    decHits :: I.MoveResult -> Hits -> HitDecreaser -> Hits
    decHits I.Miss h _ = h
    decHits I.Hit h decreaser = decreaser h

hitsPerGame :: I.GameVariation -> Int
hitsPerGame I.Classical = 20
hitsPerGame I.Tetris = 20
hitsPerGame I.TShape = 25

toNestedMoves :: Game -> I.Moves
toNestedMoves (Game c r) = toNestedMoves' (reverse r) (I.Moves (mapCoord c) Nothing Nothing)
  where
    toNestedMoves' :: [Move] -> I.Moves -> I.Moves
    toNestedMoves' [] acc = acc
    toNestedMoves' (ReplyAndAttack cor res : t) acc = toNestedMoves' t (I.Moves (mapCoord cor) (Just res) (Just acc))
    toNestedMoves' (LastReply res : _) acc = I.Moves [] (Just res) (Just acc)
    mapCoord :: (Column, Row) -> [T.Text]
    mapCoord (c, r) = [cs (show c), cs (show r)]
