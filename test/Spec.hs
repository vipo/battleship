{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Domain as D

import qualified Data.Aeson.Types as A
import qualified Data.Vector as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BattleShip Specification" [json]

json :: TestTree
json = testGroup "Json" [
  testCase "Eliminates root maps" $ 
    D.withOutMaps (A.object [("a", A.emptyArray), ("b", A.String "b")]) @?=
      (A.Array $ V.fromList [A.String "a", A.emptyArray, A.String "b", A.String "b"]),
  testCase "Eliminates nested maps" $
    D.withOutMaps (A.Array $ V.fromList [A.object [("b", A.String "b")], A.String "a"]) @?=
      (A.Array $ V.fromList [A.Array $ V.fromList [A.String "b", A.String "b"], A.String "a"])]
	  
