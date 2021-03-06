{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ApiTypes
import Domain
import Interface

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Database.Redis (Connection, checkedConnect, connectHost, defaultConnectInfo)

import Network.Wai.Handler.Warp
import Servant
import Data.String.Conversions

import System.IO
import System.Environment(lookupEnv)

server :: Connection -> Server API
server redis = arbitrary :<|> echo :<|> listGames :<|> gamePage :<|> runGame
  where
    arbitrary :: GameVariation -> Maybe Int -> Handler Moves
    arbitrary variation seed = liftIO $ arbitraryGame variation seed
    echo :: Moves -> Handler Moves
    echo = return
    gamePage :: GameId -> GamePage -> Handler Moves
    gamePage gid page = do
      r <- liftIO $ getGamePage redis gid page
      case r of
        Just v -> return v
        Nothing -> throwError $ err404 {errBody = "Not found"}
    listGames :: Handler GameStats
    listGames = liftIO $ getStats redis
    runGame gid pid = postMove :<|> getMove
      where
        postMove :: Moves -> Handler NoContent
        postMove ms = orConflict $ fmap (const NoContent) <$> saveMove redis gid pid ms
        getMove :: Handler Moves
        getMove = orConflict $ fetchMove redis gid pid

orConflict :: IO (Either MoveErr a) -> Handler a
orConflict res = do
  r <- liftIO res
  case r of
    Left (ContractErr msg) -> throwError $ err409 {errBody = cs msg}
    Left (ParseErr msg) -> throwError $ err400 {errBody = cs msg}
    Left (SystemErr msg) -> throwError $ err500 {errBody = cs msg}
    Right a -> return a

api :: Proxy API
api = Proxy

app :: Connection -> Application
app redis = serve api (server redis)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  bracket
    (do
      redisHost <- fromMaybe "redis" <$> lookupEnv "REDIS_HOST"
      let connInfo = defaultConnectInfo {connectHost = redisHost}
      conn <- checkedConnect connInfo
      putStrLn $ "Connected to: " ++ show connInfo
      return conn)
    (\_ -> return ())
    (\redis -> do
       putStrLn $ "Starting on port " ++ show port
       run port (app redis))
  where
    port = 8080
