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

data JSONNoLists
instance Accept JSONNoLists where
  contentType _ = "application" // "json+nolists"
instance ToJSON a => MimeRender JSONNoLists a where
  mimeRender _ = encode . D.withOutLists . toJSON
  
data JSONNoMaps
instance Accept JSONNoMaps where
  contentType _ = "application" // "json+nomaps"
instance ToJSON a => MimeRender JSONNoMaps a where
  mimeRender _ = encode . D.withOutMaps . toJSON

type API =
  "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Integer :> Get '[JSON, JSONNoLists, JSONNoMaps] I.Moves

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
