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
import Data.String.Conversions

import System.IO

server :: Connection -> Server API
server redis = arbitrary :<|> echo :<|> runGame
  where
    arbitrary :: GameVariation -> Maybe Int -> Handler Moves
    arbitrary variation seed = liftIO $ arbitraryGame variation seed
    echo :: Moves -> Handler Moves
    echo = return
    runGame gid pid = postMove :<|> getMove
      where
        postMove :: Moves -> Handler NoContent
        postMove moves = orConflict $ fmap (const NoContent) <$> saveMove redis gid pid moves
        getMove :: Handler Moves
        getMove = orConflict $ fetchMove redis gid pid

orConflict :: IO (Either String a) -> Handler a
orConflict res = do
  r <- liftIO res
  case r of
    Left msg -> throwError $ err409 {errBody = cs msg}
    Right a -> return a

api :: Proxy API
api = Proxy

app :: Connection -> Application
app redis = serve api (server redis)

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
