{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ApiTypes where

import qualified Interface as I

import qualified Data.Aeson as A
import qualified Data.BEncode as Ben
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text as T
import Network.HTTP.Media((//), MediaType)
import Servant
import TextShow 

import Data.String.Conversions

instance FromHttpApiData I.GameVariation where
  parseUrlPiece "classical" = Right I.Classical
  parseUrlPiece "tetris" = Right I.Tetris
  parseUrlPiece "t-shapes" = Right I.TShape
  parseUrlPiece a = Left $ T.concat ["Unknown game: ", showt a]

instance FromHttpApiData I.PlayerId where
  parseUrlPiece "A" = Right I.A
  parseUrlPiece "B" = Right I.B
  parseUrlPiece a = Left $ T.concat ["Unknown player: ", showt a]

instance FromHttpApiData I.GameId where
  parseUrlPiece v = Right $ I.GameId $ cs v

instance FromHttpApiData I.GamePage where
  parseUrlPiece v = case (parseUrlPiece v :: Either T.Text Int) of
    Left e -> Left e
    Right i -> if i >=0 then Right (I.GamePage i) else Left $ cs $ "negative page" ++ show i

ct :: BS.ByteString
ct = "application"

bencMt :: BS.ByteString -> MediaType
bencMt suff = ct // BS.concat ["relaxed-bencoding", suff]

jsonMt :: BS.ByteString -> MediaType
jsonMt suff = ct // BS.concat ["json", suff]

data MyJSON
instance Accept MyJSON where
  contentType _ = jsonMt ""
instance I.ToJsonLike a => MimeRender MyJSON a where
  mimeRender _ = A.encode . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender MyJSON a where
  mimeUnrender _ bs = A.eitherDecode' bs >>= I.fromJsonLike

data JSONNoLists
instance Accept JSONNoLists where
  contentType _ = jsonMt "+nolists"
instance I.ToJsonLike a => MimeRender JSONNoLists a where
  mimeRender _ = A.encode . I.withOutLists . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender JSONNoLists a where
  mimeUnrender _ bs = A.eitherDecode' bs >>= I.fromWithOutLists >>= I.fromJsonLike

data JSONNoMaps
instance Accept JSONNoMaps where
  contentType _ = jsonMt "+nomaps"
instance I.ToJsonLike a => MimeRender JSONNoMaps a where
  mimeRender _ = A.encode . I.withOutMaps . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender JSONNoMaps a where
  mimeUnrender _ bs = A.eitherDecode' bs >>= I.fromWithOutMaps >>= I.fromJsonLike

data Bencoding
instance Accept Bencoding where
  contentType _ = bencMt ""
instance I.ToJsonLike a => MimeRender Bencoding a where
  mimeRender _ = Ben.encode . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender Bencoding a where
  mimeUnrender _ bs = Ben.decode (BLS.toStrict bs) >>= I.fromJsonLike

data BencodingNoLists
instance Accept BencodingNoLists where
  contentType _ = bencMt "+nolists"
instance I.ToJsonLike a => MimeRender BencodingNoLists a where
  mimeRender _ = Ben.encode . I.withOutLists . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender BencodingNoLists a where
  mimeUnrender _ bs = Ben.decode (BLS.toStrict bs) >>= I.fromWithOutLists >>= I.fromJsonLike

data BencodingNoMaps
instance Accept BencodingNoMaps where
  contentType _ = bencMt "+nomaps"
instance I.ToJsonLike a => MimeRender BencodingNoMaps a where
  mimeRender _ = Ben.encode . I.withOutMaps . I.mix . I.toJsonLike
instance I.FromJsonLike a => MimeUnrender BencodingNoMaps a where
  mimeUnrender _ bs = Ben.decode (BLS.toStrict bs) >>= I.fromWithOutMaps >>= I.fromJsonLike

type ContentTypes = '[MyJSON, JSONNoLists, JSONNoMaps, Bencoding, BencodingNoLists, BencodingNoMaps]

type API =
       "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Int :> Get ContentTypes I.Moves
  :<|> "game" :> "translate" :> ReqBody ContentTypes I.Moves :> Post ContentTypes I.Moves
  :<|> "game" :> Get '[JSON] I.GameStats
  :<|> "game" :> Capture "gid" I.GameId :> Capture "page" I.GamePage :> Get '[JSON] I.Moves
  :<|> "game" :> Capture "gid" I.GameId :> "player" :> Capture "pid" I.PlayerId :> (
           ReqBody ContentTypes I.Moves :> PostNoContent '[PlainText] NoContent 
      :<|> Get ContentTypes I.Moves
    )
