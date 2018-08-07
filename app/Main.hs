{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Domain as D
import qualified Interface as I

import Control.Monad.IO.Class

import Data.Aeson()
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import System.Random
import TextShow 

import Web.HttpApiData

instance FromHttpApiData I.GameVariation where
  parseUrlPiece "classic" = Right I.Classic
  parseUrlPiece "tetris" = Right I.Tetris
  parseUrlPiece "t-shapes" = Right I.TShape
  parseUrlPiece a = Left $ T.concat ["Unknown game: ", showt a]

type API =
  "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> QueryParam "seed" Integer :> Get '[JSON] I.NestedMoves

server :: Server API
server = nested
  where
    nested :: I.GameVariation -> Maybe Integer -> Handler I.NestedMoves
    nested variation seed = liftIO $ D.arbitraryGame variation seed 

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
