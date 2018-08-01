{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Domain as D
import qualified Interface as I

import Control.Monad.IO.Class

import Data.Aeson()
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import TextShow 

import Web.HttpApiData

instance FromHttpApiData I.GameVariation where
  parseUrlPiece "classic" = Right I.Classic
  parseUrlPiece "tetris" = Right I.Tetris
  parseUrlPiece "t-shapes" = Right I.TShape
  parseUrlPiece a = Left $ T.concat ["Unknown game: ", showt a]

type API =
  "game" :> Capture "variation" I.GameVariation :> "arbitrary" :> Get '[JSON] I.NestedMoves

server :: Server API
server = arbitrary
  where
    arbitrary :: I.GameVariation -> Handler I.NestedMoves
    arbitrary = liftIO . D.arbitraryGame

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
