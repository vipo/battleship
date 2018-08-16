{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Elm (Spec (Spec), specsToDir, toElmDecoderSource, toElmTypeSource)
import GHC.Generics
import GHC.TypeLits
import Servant.Elm  (ElmType, Proxy (Proxy), defElmImports, generateElmForAPI)

import qualified Interface as I
import qualified ApiTypes as Api

instance ElmType I.Moves
instance ElmType I.GameVariation
instance ElmType I.MoveResult

spec :: Spec
spec = Spec ["Api"] (defElmImports :
  toElmTypeSource (Proxy :: Proxy I.Moves) :
  toElmDecoderSource (Proxy :: Proxy I.Moves):
  generateElmForAPI  (Proxy :: Proxy Api.API))

main :: IO ()
main = specsToDir [spec] "elm"