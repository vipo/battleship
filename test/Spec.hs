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
json = testGroup "Json" [jsonNoMaps, jsonNoLists]

jsonNoMaps :: TestTree
jsonNoMaps = testGroup "Eliminate maps" [
  testCase "root" $ 
    D.withOutMaps (A.object [("a", A.emptyArray), ("b", A.String "b")]) @?=
      jarray [A.String "a", A.emptyArray, A.String "b", A.String "b"],
  testCase "nested" $
    D.withOutMaps (jarray [A.object [("b", A.String "b")], A.String "a"]) @?=
      jarray [jarray [A.String "b", A.String "b"], A.String "a"]]

jsonNoLists :: TestTree
jsonNoLists = testGroup "Eliminate lists" [
  testCase "root" $ 
    D.withOutLists (jarray [A.emptyArray, A.String "b"]) @?=
      A.object [("1", A.object []), ("2", A.String "b")],
  testCase "nested" $
    D.withOutLists (A.object [("b", A.String "b"), ("a", jarray[A.String "a", A.String "b"])]) @?=
      A.object [("b", A.String "b"), ("a", A.object[("1", A.String "a"), ("2", A.String "b")])]]


jarray :: [A.Value] -> A.Value
jarray = A.Array . V.fromList
