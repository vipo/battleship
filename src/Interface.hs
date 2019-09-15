{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Interface
  ( JsonLikeValue(..)
  , ToJsonLike(..)
  , FromJsonLike(..)
  , Moves(..)
  , MoveResult(..)
  , GameVariation(..)
  , PlayerId(..)
  , GameId(..)
  , GameStats(..)
  , GamePage(..)
  , withOutLists
  , withOutMaps
  , fromWithOutLists
  , fromWithOutMaps
  ) where

import qualified Data.Aeson.Types as A
import qualified Data.Aeson as Aes
import qualified Data.Aeson.Encoding as AE

import           Data.BEncode
import qualified Data.BEncode.BDict as BDict

import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import           Data.Text
import qualified GHC.Generics as Gen

import qualified Data.Text as T
import qualified TextShow as TS

import           Control.Monad.Except
import           Control.Monad.State

import           Type.Reflection
import           Data.String.Conversions

data GameStats = GameStats
  { games :: [GameId]
  , moves :: Integer
  } deriving (Gen.Generic, Show)
instance A.ToJSON GameStats

newtype GameId = GameId String
  deriving Gen.Generic
instance Show GameId where
  show (GameId s) = s
instance A.ToJSON GameId

data PlayerId = A | B
  deriving Show

newtype GamePage = GamePage Int
  deriving Show

data GameVariation
  = Classical
  | Tetris
  | TShape
  deriving (Gen.Generic)

data MoveResult
  = Miss
  | Hit
  deriving (Eq, Show, Gen.Generic, Typeable)

instance A.ToJSON MoveResult where
  toJSON Miss = A.String "MISS"
  toJSON Hit = A.String "HIT"

instance A.FromJSON MoveResult where
  parseJSON (A.String "MISS") = return Miss
  parseJSON (A.String "HIT") = return Hit
  parseJSON invalid = A.typeMismatch "MoveResult" invalid

data Moves = Moves
  { coord :: [Text]
  , result :: Maybe MoveResult
  , prev :: Maybe Moves
  } deriving (Gen.Generic, Show, Eq, Typeable)

instance A.ToJSON Moves where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }
instance A.FromJSON Moves where
  parseJSON = A.genericParseJSON A.defaultOptions { A.omitNothingFields = True }

data JsonLikeValue
  = JLMap [(Text, JsonLikeValue)]
  | JLArray [JsonLikeValue]
  | JLString Text
  deriving (Gen.Generic, Show, Eq, Typeable)

instance BEncode JsonLikeValue where
  toBEncode (JLString t) = BString (cs t)
  toBEncode (JLArray arr) = BList $ L.map toBEncode arr
  toBEncode (JLMap m) = BDict $ BDict.fromAscList $ L.map (\(k, v) -> (cs k, toBEncode v)) m

  fromBEncode (BString t) = Right $ JLString $ cs t
  fromBEncode (BList arr) = JLArray <$> traverse fromBEncode arr
  fromBEncode (BDict d) = JLMap <$> sequenceA l
      where
        l :: [Result (Text, JsonLikeValue)]
        l = L.map (\(k, v) -> fmap (cs k,) (fromBEncode v)) (BDict.toAscList d)
  fromBEncode a = decodingError $ "Not supported: " ++ show a

instance A.FromJSON JsonLikeValue where
  parseJSON (Aes.Object o) = JLMap <$> sequenceA (
      HMS.foldlWithKey' (\a k v -> fmap (k,) (Aes.parseJSON v) : a) [] o)
  parseJSON (Aes.Array a)  = JLArray <$> traverse Aes.parseJSON (V.toList a)
  parseJSON (Aes.String t) = return $ JLString t
  parseJSON v              = A.typeMismatch "Not expected" v

instance A.ToJSON JsonLikeValue where
  toEncoding (JLString txt) = AE.text txt
  toEncoding (JLArray arr) = AE.list A.toEncoding arr
  toEncoding (JLMap arr) =
    case arr of
      [] -> AE.null_
      (h : t) -> nonEmptyObject (toSeries h) t
    where
      toSeries :: (Text, JsonLikeValue) -> AE.Series
      toSeries (k, v) = AE.pair k (Aes.toEncoding v)
      nonEmptyObject :: AE.Series -> [(Text, JsonLikeValue)] -> A.Encoding
      nonEmptyObject s [] = A.pairs s
      nonEmptyObject s (h : t) = nonEmptyObject (s <> toSeries h) t

class FromJsonLike a where
  fromJsonLike :: JsonLikeValue -> Either String a

instance FromJsonLike Moves where
  fromJsonLike (JLMap m) =
    Moves <$> lookupMandatory "coord" <*> lookupMaybe "result" <*> lookupMaybe "prev"
    where
      lookupMaybe :: FromJsonLike a => Text -> Either String (Maybe a)
      lookupMaybe key = case L.lookup key m of
                          Nothing -> Right Nothing
                          Just v -> Just <$> fromJsonLike v
      lookupMandatory :: FromJsonLike a => Text -> Either String a
      lookupMandatory key = case L.lookup key m of
                              Nothing -> Left $ cs key ++" expected"
                              Just c -> fromJsonLike c
  fromJsonLike a = notExpected a

instance FromJsonLike Text where
  fromJsonLike (JLString t) = Right t
  fromJsonLike a = notExpected a

instance FromJsonLike a => FromJsonLike [a] where
  fromJsonLike (JLArray a) = traverse fromJsonLike a
  fromJsonLike a = notExpected a 

instance FromJsonLike MoveResult where
  fromJsonLike (JLString "MISS") = Right Miss
  fromJsonLike (JLString "HIT")  = Right Hit
  fromJsonLike a = notExpected a

notExpected :: JsonLikeValue -> Either String a
notExpected key = Left $ "Not expected: " ++ show key

class ToJsonLike a where
  toJsonLike :: a -> JsonLikeValue

instance ToJsonLike Text where
  toJsonLike = JLString

instance ToJsonLike a => ToJsonLike [a] where
  toJsonLike l = JLArray $ L.map toJsonLike l

instance ToJsonLike MoveResult where
  toJsonLike Miss = JLString "MISS"
  toJsonLike Hit =  JLString "HIT"

instance ToJsonLike Moves where
  toJsonLike Moves{..} = JLMap $
    [ ("coord", toJsonLike coord) ] ++
    M.maybeToList (fmap (\v -> ("result", toJsonLike v)) result) ++
    M.maybeToList (fmap (\v -> ("prev",   toJsonLike v)) prev)

lexOrdered :: Int -> [T.Text]
lexOrdered a = L.sort $ L.map TS.showt [1 .. a]

withOutLists :: JsonLikeValue -> JsonLikeValue
withOutLists (JLMap m) = JLMap $ L.map (\(k, v) -> (k, withOutLists v)) m
withOutLists (JLArray v) = JLMap $ L.zip (lexOrdered (L.length v)) (L.map withOutLists v)
withOutLists a = a

withOutMaps :: JsonLikeValue -> JsonLikeValue
withOutMaps (JLMap m) = JLArray $ L.concatMap (\(k, v) -> [JLString k, withOutMaps v]) m
withOutMaps (JLArray v) = JLArray $ L.map withOutMaps v
withOutMaps a = a

fromWithOutMaps :: JsonLikeValue -> Either String JsonLikeValue
fromWithOutMaps d = runCompM d comp
  where
    comp :: MapM JsonLikeValue
    comp = do
      curr <- currentNode
      n <- listAsMap curr
      let result = JLMap $ Map.toList n
      case Map.lookup "prev" n of
        Nothing -> return result
        Just p -> recurseM p comp >>= (\v -> return (JLMap (Map.toList (Map.insert "prev" v n))))

fromWithOutLists :: JsonLikeValue -> Either String JsonLikeValue
fromWithOutLists d = runCompM d comp
  where
    comp :: MapM JsonLikeValue
    comp = do
      coord <- lookupInJLMap "coord"
      cm <- asMap coord
      insertInJLMap "coord" $ JLArray $ L.map snd $ L.sortBy (\(a, _) (b, _) -> compare a b) (Map.toList cm)
      prev <- safeLookupInJLMap "prev"
      case prev of
        Nothing -> currentNode
        Just p -> recurseM p comp >>= insertInJLMap "prev" >> currentNode

recurseM :: JsonLikeValue -> MapM JsonLikeValue -> MapM JsonLikeValue
recurseM d comp =
  case runCompM d comp of
    Right r -> return r
    Left m -> throwError m

type MapM a = ExceptT String (State JsonLikeValue) a

runCompM :: JsonLikeValue -> MapM JsonLikeValue -> Either String JsonLikeValue
runCompM st comp = evalState (runExceptT comp) st

currentNode :: MapM JsonLikeValue
currentNode = lift get

safeLookupInJLMap :: Text -> MapM (Maybe JsonLikeValue)
safeLookupInJLMap k = do
  jlv <- lift get
  case jlv of
    JLMap m -> return $ Map.lookup k (Map.fromList m)
    a -> throwError $ L.concat ["Map expected for safe lookup of ", show k, ", found: ", show a]

lookupInJLMap :: Text -> MapM JsonLikeValue
lookupInJLMap k = do
  jlv <- lift get
  case jlv of
    JLMap m ->
      case Map.lookup k (Map.fromList m) of
        Just v -> return v
        Nothing -> throwError $ "Key not found: " ++ show k
    a -> throwError $ L.concat ["Map expected for lookup of ", show k, ", found: ", show a]

insertInJLMap :: Text -> JsonLikeValue -> MapM ()
insertInJLMap k v = do
  jlv <- lift get
  case jlv of
    JLMap m -> lift $ put $ JLMap $ Map.toList $ Map.insert k v (Map.fromList m)
    a -> throwError $ "Map expected on insert, found: " ++ show a

asMap :: JsonLikeValue -> MapM (Map.Map Text JsonLikeValue)
asMap (JLMap m) = return $ Map.fromList m
asMap a = throwError $ "Map expected, found: " ++ show a

listAsMap :: JsonLikeValue -> MapM (Map.Map Text JsonLikeValue)
listAsMap (JLArray a) = readKey a $ Map.fromList []
  where
    readKey :: [JsonLikeValue]
      -> Map.Map Text JsonLikeValue
      -> MapM (Map.Map Text JsonLikeValue)
    readKey [] acc = return acc
    readKey (JLString s:t) acc = readValue s t acc
    readKey o _ = throwError $ "A key value expected, found: " ++ show o
    readValue :: Text
      -> [JsonLikeValue]
      -> Map.Map Text JsonLikeValue
      -> MapM (Map.Map Text JsonLikeValue)
    readValue k [] _ = throwError $ "A value was expected for key: " ++ show k
    readValue k (h:t) acc = readKey t $ Map.insert k h acc
listAsMap a = throwError $ "List expected, found: " ++ show a
