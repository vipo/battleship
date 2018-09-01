{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Bencoding where

import Data.BEncode
import qualified Data.BEncode.Internal as BenI
import qualified Data.BEncode.BDict as BDict

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS

import Interface as I

import Data.String.Conversions

instance BEncode MoveResult where
  toBEncode Miss = BString "MISS"
  toBEncode Hit = BString "HIT"
  fromBEncode (BString "MISS") = Right Miss
  fromBEncode (BString "HIT") = Right Hit
  fromBEncode a = decodingError $ "Unknown MoveResult: " ++ show a

instance BEncode Moves where
  toBEncode Moves {..} = toDict $
    "coord" .=! map toBEncode coord .:
    "prev" .=? prev .:
    "result" .=? result .:
    endDict
  fromBEncode = getMoves
    where
      getMoves :: BValue -> Result Moves
      getMoves (BDict dict) = do
        r <- optField dict "result"
        p <- optField dict "prev"
        c <- reqField dict "coord"
        cc <- mapM coordValue c
        return $ Moves cc r p
      getMoves a = decodingError $ "dict expected, found: " ++ show a
      optField :: BEncode a => BDict.BDictMap BValue-> BDict.BKey -> Result (Maybe a)
      optField dict key = case BDict.lookup key dict of
        Nothing -> Right Nothing
        Just v -> Just <$> fromBEncode v
      reqField :: BEncode a => BDict.BDictMap BValue-> BDict.BKey -> Result a
      reqField dict key = case BDict.lookup key dict of
        Nothing -> decodingError $ "mandatory field not found: " ++ cs key
        Just v -> fromBEncode v
      coordValue :: BValue -> Result T.Text
      coordValue (BString s) = Right $ cs s
      coordValue a = decodingError $ "value not expected: " ++ show a 
  
instance JsonLike BValue where
  toJsonLike (BDict m) = JLMap $ map (\(k, v) -> (cs k, toJsonLike v)) (BDict.toAscList m)
  toJsonLike (BList v) = JLArray $ map toJsonLike v
  toJsonLike (BString t) = JLString $ cs t
  toJsonLike a = JLRaw a

  fromJsonLike (JLMap m) = BDict $ BDict.fromAscList $ L.map (\(k, v) -> (cs k, fromJsonLike v)) m
  fromJsonLike (JLArray v) = BList $ L.map fromJsonLike v
  fromJsonLike JLNull = BDict $ BDict.fromAscList []
  fromJsonLike (JLString t) = BString $ cs t
  fromJsonLike (JLRaw a) = a

  stringKey = JLRaw . BString . cs

withOutLists :: BValue -> BValue
withOutLists = fromJsonLike . I.withOutLists . toJsonLike

fromWithOutLists :: BEncode a => BLS.ByteString -> Either String a
fromWithOutLists v = do
  val <- BenI.parse (BLS.toStrict v) 
  transformed <- I.fromWithOutLists (toJsonLike val)
  let b = fromJsonLike transformed
  fromBEncode b

fromWithOutMaps :: BEncode a => BLS.ByteString -> Either String a
fromWithOutMaps v = do
  val <- BenI.parse (BLS.toStrict v) 
  transformed <- I.fromWithOutMaps (toJsonLike val)
  let b = fromJsonLike transformed
  fromBEncode b

withOutMaps :: BValue -> BValue
withOutMaps = fromJsonLike . I.withOutMaps . toJsonLike