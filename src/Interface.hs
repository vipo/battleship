{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Interface
  ( JsonLike(..)
  , JsonLikeValue(..)
  , Moves(..)
  , MoveResult(..)
  , GameVariation(..)
  , withOutLists
  , withOutMaps
  , fromWithOutLists
  , fromWithOutMaps
  ) where

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import Data.Text
import qualified GHC.Generics as Gen
import Text.Printf

import qualified Data.Text as T
import qualified TextShow as TS

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Type.Reflection

data GameVariation
  = Classical
  | Tetris
  | TShape
  deriving (Gen.Generic)

data MoveResult
  = Miss
  | Hit
  deriving (Eq, Show, Gen.Generic, Typeable)

data Moves = Moves
  { coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
  } deriving (Gen.Generic, Show, Eq, Typeable)

data JsonLikeValue a
  = JLMap [(Text, JsonLikeValue a)]
  | JLArray [JsonLikeValue a]
  | JLString Text
  | JLNull
  | JLRaw a
  deriving (Show)

class Show a =>
      JsonLike a
  where
  toJsonLike :: a -> JsonLikeValue a
  fromJsonLike :: JsonLikeValue a -> a
  stringKey :: Text -> JsonLikeValue a

lexOrdered :: Int -> [T.Text]
lexOrdered a = L.sort $ L.map TS.showt [1 .. a]

withOutLists :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutLists (JLMap m) = JLMap $ L.map (\(k, v) -> (k, withOutLists v)) m
withOutLists (JLArray v) = JLMap $ L.zip (lexOrdered (L.length v)) (L.map withOutLists v)
withOutLists a = a

withOutMaps :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutMaps (JLMap m) = JLArray $ L.concatMap (\(k, v) -> [stringKey k, withOutMaps v]) m
withOutMaps (JLArray v) = JLArray $ L.map withOutMaps v
withOutMaps a = a

fromWithOutMaps :: JsonLike a => JsonLikeValue a -> Either String (JsonLikeValue a)
fromWithOutMaps d = runCompM d comp
  where
    comp :: JsonLike a1 => MapM a1 (JsonLikeValue a1)
    comp = do
      curr <- currentNode
      n <- listAsMap curr
      let result = JLMap $ Map.toList n
      case Map.lookup "prev" n of
        Nothing -> return result
        Just JLNull -> return result
        Just p -> recurseM p comp >>= (\d -> return (JLMap (Map.toList (Map.insert "prev" d n))))

fromWithOutLists :: JsonLike a => JsonLikeValue a -> Either String (JsonLikeValue a)
fromWithOutLists d = runCompM d comp
  where
    comp :: JsonLike a1 => MapM a1 (JsonLikeValue a1)
    comp = do
      coord <- lookupInJLMap "coord"
      cm <- asMap coord
      insertInJLMap "coord" $ JLArray $ L.map snd $ L.sortBy (\(a, _) (b, _) -> compare a b) (Map.toList cm)
      prev <- safeLookupInJLMap "prev"
      case prev of
        Nothing -> currentNode
        Just JLNull -> currentNode
        Just p -> recurseM p comp >>= insertInJLMap "prev" >> currentNode

recurseM :: JsonLike a => JsonLikeValue a -> MapM a (JsonLikeValue a) -> MapM a (JsonLikeValue a)
recurseM d comp =
  case runCompM d comp of
    Right r -> return r
    Left m -> throwError m

type MapM v a = ExceptT String (State (JsonLikeValue v)) a

runCompM :: JsonLike a => JsonLikeValue a -> MapM a (JsonLikeValue a) -> Either String (JsonLikeValue a)
runCompM state comp = evalState (runExceptT comp) state

currentNode :: JsonLike a => MapM a (JsonLikeValue a)
currentNode = lift get

safeLookupInJLMap :: JsonLike a => Text -> MapM a (Maybe (JsonLikeValue a))
safeLookupInJLMap k = do
  jlv <- lift get
  case jlv of
    JLMap m -> return $ Map.lookup k (Map.fromList m)
    a -> throwError $ L.concat ["Map expected for safe lookup of ", show k, ", found: ", show a]

lookupInJLMap :: JsonLike a => Text -> MapM a (JsonLikeValue a)
lookupInJLMap k = do
  jlv <- lift get
  case jlv of
    JLMap m ->
      case Map.lookup k (Map.fromList m) of
        Just v -> return v
        Nothing -> throwError $ "Key not found: " ++ show k
    a -> throwError $ L.concat ["Map expected for lookup of ", show k, ", found: ", show a]

insertInJLMap :: JsonLike a => Text -> JsonLikeValue a -> MapM a ()
insertInJLMap k v = do
  jlv <- lift get
  case jlv of
    JLMap m -> lift $ put $ JLMap $ Map.toList $ Map.insert k v (Map.fromList m)
    a -> throwError $ "Map expected on insert, found: " ++ show a

asMap :: JsonLike a => JsonLikeValue a -> MapM a (Map.Map Text (JsonLikeValue a))
asMap (JLMap m) = return $ Map.fromList m
asMap a = throwError $ "Map expected, found: " ++ show a

listAsMap :: JsonLike a => JsonLikeValue a -> MapM a (Map.Map Text (JsonLikeValue a))
listAsMap (JLArray a) = readKey a $ Map.fromList []
  where
    readKey ::
         JsonLike a1
      => [JsonLikeValue a1]
      -> Map.Map Text (JsonLikeValue a1)
      -> MapM a1 (Map.Map Text (JsonLikeValue a1))
    readKey [] acc = return acc
    readKey (JLString s:t) acc = readValue s t acc
    readKey a _ = throwError $ "A key value expected, found: " ++ show a
    readValue ::
         JsonLike a2
      => Text
      -> [JsonLikeValue a2]
      -> Map.Map Text (JsonLikeValue a2)
      -> MapM a2 (Map.Map Text (JsonLikeValue a2))
    readValue k [] _ = throwError $ "A value was expected for key: " ++ show k
    readValue k (h:t) acc = readKey t $ Map.insert k h acc
listAsMap a = throwError $ "List expected, found: " ++ show a
