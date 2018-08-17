{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Main where

import ApiTypes
import Domain
import Interface

import Control.Monad.IO.Class

import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Media((//), MediaType)
import Network.Wai.Handler.Warp
import Servant

import Paths_battleship

server :: Server API
server = arbitrary :<|> serveDirectoryWebApp "static"
  where
    arbitrary :: GameVariation -> Maybe Int -> Handler Moves
    arbitrary variation seed = liftIO $ arbitraryGame variation seed

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
