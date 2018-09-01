{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module ApiTypes where

import qualified Domain as D
import qualified Interface as I
import Json as J
import Bencoding as B

import qualified Data.Aeson as A
import qualified Data.BEncode as Ben
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Media((//), MediaType)
import Servant
import Servant.API.ContentTypes (Accept)
import TextShow 

import Web.HttpApiData

instance FromHttpApiData I.GameVariation where
  parseUrlPiece "classical" = Right I.Classical
  parseUrlPiece "tetris" = Right I.Tetris
  parseUrlPiece "t-shapes" = Right I.TShape
  parseUrlPiece a = Left $ T.concat ["Unknown game: ", showt a]

ct :: BS.ByteString
ct = "application"

bencMt :: BS.ByteString -> MediaType
bencMt suff = ct // BS.concat ["bencoding", suff]

jsonMt :: BS.ByteString -> MediaType
jsonMt suff = ct // BS.concat ["json", suff]

data JSONNoLists
instance Accept JSONNoLists where
  contentType _ = jsonMt "+nolists"
instance A.ToJSON a => MimeRender JSONNoLists a where
  mimeRender _ = A.encode . J.withOutLists . A.toJSON
instance A.FromJSON a => MimeUnrender JSONNoLists a where
  mimeUnrender _ = J.fromWithOutLists
  
data JSONNoMaps
instance Accept JSONNoMaps where
  contentType _ = jsonMt "+nomaps"
instance A.ToJSON a => MimeRender JSONNoMaps a where
  mimeRender _ = A.encode . J.withOutMaps . A.toJSON
instance A.FromJSON a => MimeUnrender JSONNoMaps a where
  mimeUnrender _ = J.fromWithOutMaps

data Bencoding
instance Accept Bencoding where
  contentType _ = bencMt ""
instance Ben.BEncode a => MimeRender Bencoding a where
  mimeRender _ = Ben.encode . Ben.toBEncode
instance Ben.BEncode a => MimeUnrender Bencoding a where
  mimeUnrender _ = Ben.decode . BLS.toStrict

data BencodingNoLists
instance Accept BencodingNoLists where
  contentType _ = bencMt "+nolists"
instance Ben.BEncode a => MimeRender BencodingNoLists a where
  mimeRender _ = Ben.encode . B.withOutLists . Ben.toBEncode
instance Ben.BEncode a => MimeUnrender BencodingNoLists a where
  mimeUnrender _ = B.fromWithOutLists

data BencodingNoMaps
instance Accept BencodingNoMaps where
  contentType _ = bencMt "+nomaps"
instance Ben.BEncode a => MimeRender BencodingNoMaps a where
  mimeRender _ = Ben.encode . B.withOutMaps . Ben.toBEncode
instance Ben.BEncode a => MimeUnrender BencodingNoMaps a where
  mimeUnrender _ = B.fromWithOutMaps

type API =
       "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Int :>
         Get  '[JSON, JSONNoLists, JSONNoMaps, Bencoding, BencodingNoLists, BencodingNoMaps] I.Moves
  :<|> "game" :> "translate" :> ReqBody '[JSON, JSONNoLists, JSONNoMaps, Bencoding, BencodingNoLists, BencodingNoMaps] I.Moves :>
         Post '[JSON, JSONNoLists, JSONNoMaps, Bencoding, BencodingNoLists, BencodingNoMaps] I.Moves
  :<|> "static" :> Raw
