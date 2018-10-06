{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain
  ( arbitraryGame
  , toNestedMoves
  , fromNestedMoves
  , saveMove
  , fetchMove
  , getStats
  , getGamePage
  , MoveErr(..)
  , Column(..)
  , Row(..)
  , Move(..)
  , Game(..)
  ) where

import Boards

import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified GHC.Generics as Gen
import qualified Interface as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Either
import qualified Data.Text as T
import qualified TextShow as TS

import Data.Maybe
import Data.String.Conversions
import qualified Database.Redis as R

import System.Random (RandomGen, getStdGen, mkStdGen, next, split)
import System.Random.Shuffle (shuffle')

import qualified Text.Read as Read

data Game = Game
  { firstAttack :: Coordinates
  , replies :: [Move]
  } deriving (Gen.Generic, Show, Eq)
instance FromJSON Game
instance ToJSON Game

data Move
  = ReplyAndAttack Coordinates I.MoveResult
  | LastReply I.MoveResult
  deriving (Gen.Generic, Show, Eq)
instance FromJSON Move
instance ToJSON Move

type Seed = Int

data CS = forall a. CoordinatesSet a => CS a

data MoveErr
  = ContractErr String
  | ParseErr String
  | SystemErr String

anotherPlayer :: I.PlayerId -> I.PlayerId
anotherPlayer I.A = I.B
anotherPlayer I.B = I.A

mailBox :: I.GameId -> I.PlayerId -> BS.ByteString
mailBox gid pid = cs $ "mailbox:" ++ show gid ++ ":" ++ show pid

counterKey :: BS.ByteString
counterKey = "counter"

gamesKey :: BS.ByteString
gamesKey = "games"

historyKey :: I.GameId -> BS.ByteString
historyKey gid = cs $ "history:" ++ show gid

getGamePage :: R.Connection -> I.GameId -> I.GamePage -> IO (Maybe I.Moves)
getGamePage redis gid (I.GamePage page) = R.runRedis redis $ do
  els <- R.lrange (historyKey gid) (toInteger page) (toInteger page)
  return $ case els of
    Right [bs] -> toNestedMoves <$> A.decode (BSL.fromStrict bs)
    _ -> Nothing

getStats :: R.Connection -> IO I.GameStats
getStats redis = R.runRedis redis $ do
  c <- R.get counterKey
  let counter =
        case c of
          Right (Just v) -> read $ cs v
          _ -> 0 :: Integer
  range <- R.zrevrangebyscoreLimit gamesKey (fromIntegral counter) 0.0 0 42
  case range of
    Left _ -> return $ I.GameStats [] 0
    Right r -> return $ I.GameStats (map (I.GameId . cs) r) counter

fetchMove :: R.Connection -> I.GameId -> I.PlayerId -> IO (Either MoveErr I.Moves)
fetchMove redis gid pid = do
  v <- R.runRedis redis $ R.blpop [mailBox gid pid] 5
  case v of
    Right (Just (_, bs)) -> return $ readMoves bs
    _ -> return $ Left $ ContractErr "No move available at the moment"
  where
    readMoves bs =
      case A.decode $ BSL.fromStrict bs of
        Just g -> Right $ toNestedMoves g
        _ -> Left $ SystemErr "Could not parse mailbox content"

saveMove :: R.Connection -> I.GameId -> I.PlayerId -> I.Moves -> IO (Either MoveErr ())
saveMove redis gid pid moves = do
  rawState <- R.runRedis redis $ R.get stateKey
  case rawState of
    Left e -> return $ Left $ SystemErr $ show e
    Right mbs -> parseState mbs
  where
    gameOver = Left $ ContractErr "Game already finished"
    stateKey = cs $ "state:" ++ show gid
    parseState (Just bs) =
      case A.decode $ BSL.fromStrict bs of
        Just (Game _ (LastReply _:_)) -> return gameOver
        Just (Game _ rs) | length rs >= 199 -> return gameOver
        _ -> processRequest
    parseState Nothing = processRequest
    processRequest = 
      case fromNestedMoves moves of
        Left msg -> return $ Left $ ParseErr msg
        Right game -> R.runRedis redis $ do
          let encoded = cs $ A.encode game
          _ <- R.set stateKey encoded
          counterValue <- R.incr counterKey
          _ <- R.zadd gamesKey [(fromIntegral $ fromRight 0 counterValue, cs $ show gid)]
          _ <- R.rpush (historyKey gid) [encoded]
          _ <- R.rpush (cs (mailBox gid (anotherPlayer pid))) [encoded]
          return $ Right ()

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
    moves =
      zip (map (cs1, , \(a, b) -> (a - 1, b)) moves1) (map (cs2, , \(a, b) -> (a, b - 1)) moves2) >>=
      (\t -> [fst t, snd t])
    start :: [MoveState] -> Game
    start [] = error "Empty game"
    start ((b, c, d):t) = react (limit, (maxHits, maxHits)) (result c b, d) t (Game c [])
    react :: (Int, (Int, Int)) -> (I.MoveResult, HitDecreaser) -> [MoveState] -> Game -> Game
    react (0, _) _ _ acc = acc
    react _ (r, _) [] (Game f a) = Game f (LastReply r : a)
    react (l, h) (r, d) ((b, c, nd):t) (Game f a) =
      case decHits r h d of
        (h1, h2)
          | h1 <= 0 || h2 <= 0 -> Game f (LastReply r : a)
        nh -> react (l - 1, nh) (result c b, nd) t (Game f (ReplyAndAttack c r : a))
    result :: Coordinates -> CS -> I.MoveResult
    result c (CS b) =
      if b `contains` c
        then I.Hit
        else I.Miss
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
    toNestedMoves' (ReplyAndAttack cor res:t) acc = toNestedMoves' t (I.Moves (mapCoord cor) (Just res) (Just acc))
    toNestedMoves' (LastReply res:_) acc = I.Moves [] (Just res) (Just acc)
    mapCoord :: (Column, Row) -> [T.Text]
    mapCoord (c', r') = [cs (show c'), cs (show r')]

type M = ([T.Text], Maybe I.MoveResult)

fromNestedMoves :: I.Moves -> Either String Game
fromNestedMoves m =
  let toChain :: I.Moves -> [M] -> [M]
      toChain (I.Moves c r (Just p)) acc = toChain p ((c, r) : acc)
      toChain (I.Moves c r Nothing) acc = (c, r) : acc
      readCoords :: [T.Text] -> Either String Coordinates
      readCoords [c1, c2] = do
        col <- Read.readEither (cs c1)
        row <- Read.readEither (cs c2)
        return (col, row)
      readCoords _ = Left "Illegal coordinates"
      firstMove :: [M] -> Either String Game
      firstMove ((c, Nothing):ms) = do
        cr <- readCoords c
        otherMoves ms $ Game cr []
      firstMove _ = Left "Illegal start of game"
      otherMoves :: [M] -> Game -> Either String Game
      otherMoves [([], Just r)] (Game a b) = Right $ Game a (LastReply r : b)
      otherMoves ((c, Just r):ms) (Game a b) = do
        cr <- readCoords c
        otherMoves ms $ Game a (ReplyAndAttack cr r : b)
      otherMoves [] acc = Right acc
      otherMoves m _ = Left $ "Illegal move: " ++ show m
   in firstMove $ toChain m []
