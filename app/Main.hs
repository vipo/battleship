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

server :: Server API
server = arbitrary :<|> echo
  where
    arbitrary :: GameVariation -> Maybe Int -> Handler Moves
    arbitrary variation seed = liftIO $ arbitraryGame variation seed
    echo :: Moves -> Handler Moves
    echo = return

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn $ "Starting on port " ++ show port
  run port app
  where
    port = 8080
