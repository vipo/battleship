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

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.Redis (Connection, checkedConnect, connectHost, defaultConnectInfo)

import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Media (MediaType, (//))
import Network.Wai.Handler.Warp
import Servant
import Servant.API.ContentTypes

import System.IO

server :: Server API
server = arbitrary :<|> echo :<|> runGame
  where
    arbitrary :: GameVariation -> Maybe Int -> Handler Moves
    arbitrary variation seed = liftIO $ arbitraryGame variation seed
    echo :: Moves -> Handler Moves
    echo = return
    runGame gid pid = postMove :<|> getMove
      where
        postMove :: Moves -> Handler NoContent
        postMove moves = return NoContent
        getMove :: Handler Moves
        getMove = return $ Moves [] Nothing Nothing

api :: Proxy API
api = Proxy

app :: Connection -> Application
app redis = serve api server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  bracket
    (checkedConnect defaultConnectInfo {connectHost = "redis"})
    (\_ -> return ())
    (\redis -> do
       putStrLn $ "Starting on port " ++ show port
       run port (app redis))
  where
    port = 8080
