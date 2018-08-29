{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}

module Interface (
  JsonLike(..), JsonLikeValue(..),
  Moves(..), MoveResult(..), GameVariation(..), 
  withOutLists, withOutMaps, fromWithOutLists
) where

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import Data.Text
import Text.Printf
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Type.Reflection

data GameVariation = Classical | Tetris | TShape
  deriving (Gen.Generic)

data MoveResult = Miss | Hit
  deriving (Eq, Show, Gen.Generic, Typeable)

data Moves = Moves {
  coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
} deriving (Gen.Generic, Show, Eq, Typeable)

data JsonLikeValue a =
  JLMap [(Text, JsonLikeValue a)] |
  JLArray [JsonLikeValue a] |
  JLRaw a
  deriving Show

class Show a => JsonLike a where
  toJsonLike :: a -> JsonLikeValue a
  fromJsonLike :: JsonLikeValue a -> a 
  stringKey :: Text -> JsonLikeValue a

lexOrdered :: Int -> [T.Text]
lexOrdered a = L.sort $ L.map TS.showt [1..a]

withOutLists :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutLists (JLMap m) = JLMap $ L.map (\(k, v) -> (k, withOutLists v)) m
withOutLists (JLArray v) = JLMap $ L.zip (lexOrdered (L.length v)) (L.map withOutLists v)
withOutLists a = a

fromWithOutLists :: JsonLike a => JsonLikeValue a -> Either String (JsonLikeValue a)
fromWithOutLists (JLMap d) = runCompM (Map.fromList d) comp
  where
    comp :: forall a1 . JsonLike a1 => MapM (JsonLikeValue a1) (JsonLikeValue a1)
    comp = do
      coord <- lookupM "coord"
      cm <- asJLMapM coord
      insertM "coord" $ JLArray $ L.map snd $ L.sortBy (\(a,_) (b,_) -> compare a b) cm
      prev <- safeLookupM "prev"
      case prev of
        Nothing -> currentNodeM
        Just p -> currentNodeM
fromWithOutLists a = Left $ "Map expected, found: " ++ show a

withOutMaps :: JsonLike a => JsonLikeValue a -> JsonLikeValue a
withOutMaps (JLMap m) = JLArray $ L.concatMap (\(k, v) -> [stringKey k, withOutMaps v]) m
withOutMaps (JLArray v) = JLArray $ L.map withOutMaps v
withOutMaps a = a

type MapM v = ExceptT String (State (Map.Map Text v))

runCompM :: JsonLike a => Map.Map Text (JsonLikeValue a) -> MapM (JsonLikeValue a) (JsonLikeValue a) -> Either String (JsonLikeValue a)
runCompM state comp = evalState (runExceptT comp) state

currentNodeM :: JsonLike a => MapM (JsonLikeValue a) (JsonLikeValue a)
currentNodeM = do
  m <- lift get
  return $ JLMap $ Map.toList m

safeLookupM :: JsonLike a => Text -> MapM (JsonLikeValue a) (Maybe (JsonLikeValue a))
safeLookupM k = do
  m <- lift get
  return $ Map.lookup k m

lookupM :: JsonLike a => Text -> MapM (JsonLikeValue a) (JsonLikeValue a) 
lookupM k = do
  m <- lift get
  case Map.lookup k m of
    Just v -> return v
    Nothing -> throwError $ "Key not found: " ++ show k

insertM :: JsonLike a => Text -> JsonLikeValue a -> MapM (JsonLikeValue a) ()
insertM k v = do
  m <- lift get
  lift $ put $ Map.insert k v m  

asJLMapM :: JsonLike a => JsonLikeValue a -> MapM (JsonLikeValue a) [(Text, JsonLikeValue a)]
asJLMapM (JLMap m) = return m
asJLMapM a = throwError $ "Map expected, found: " ++ show a