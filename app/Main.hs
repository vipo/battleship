{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Main where

import qualified Domain as D
import qualified Interface as I
import Json
import Bencoding

import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.BEncode as B
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Media((//))
import Network.Wai.Handler.Warp
import Servant
import Servant.API.ContentTypes (Accept)
import TextShow 

import Web.HttpApiData

instance FromHttpApiData I.GameVariation where
  parseUrlPiece "classic" = Right I.Classic
  parseUrlPiece "tetris" = Right I.Tetris
  parseUrlPiece "t-shapes" = Right I.TShape
  parseUrlPiece a = Left $ T.concat ["Unknown game: ", showt a]

data JSONNoLists
instance Accept JSONNoLists where
  contentType _ = "application" // "json+nolists"
instance A.ToJSON a => MimeRender JSONNoLists a where
  mimeRender _ = A.encode . D.withOutLists . A.toJSON
  
data JSONNoMaps
instance Accept JSONNoMaps where
  contentType _ = "application" // "json+nomaps"
instance A.ToJSON a => MimeRender JSONNoMaps a where
  mimeRender _ = A.encode . D.withOutMaps . A.toJSON

data Bencoding
instance Accept Bencoding where
  contentType _ = "application" // "bencoding"
instance B.BEncode a => MimeRender Bencoding a where
  mimeRender _ = B.encode . B.toBEncode

type API =
  "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Integer :>
    Get '[JSON, JSONNoLists, JSONNoMaps, Bencoding] I.Moves

server :: Server API
server = arbitrary
  where
    arbitrary :: I.GameVariation -> Maybe Integer -> Handler I.Moves
    arbitrary variation seed = liftIO $ D.arbitraryGame variation seed 

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
