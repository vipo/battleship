{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Domain as D
import qualified Interface as I

import Control.Monad.IO.Class

import Data.Aeson(ToJSON, encode, toJSON)
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

data JSONList
instance Accept JSONList where
  contentType _ = "application" // "json+list"
instance ToJSON a => MimeRender JSONList a where
  mimeRender _ = encode . D.asListsOnly . toJSON
  
data JSONMap
instance Accept JSONMap where
  contentType _ = "application" // "json+map"
instance ToJSON a => MimeRender JSONMap a where
  mimeRender _ = encode . D.asMapsOnly . toJSON

type API =
  "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Integer :> Get '[JSON, JSONList, JSONMap] I.Moves

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
