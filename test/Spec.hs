{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Domain as D
import qualified Interface as I
import Json
import Bencoding

import qualified Data.Aeson.Types as A
import qualified Data.Aeson as Aeson
import qualified Data.BEncode as Ben
import qualified Data.Vector as V

import Data.String.Conversions

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BattleShip Specification" [common, json]

json :: TestTree
json = testGroup "Json" [jsonNoMaps, jsonNoLists]

someGame :: D.Game
someGame = D.Game
  (D.A, D.R1)
  [D.ReplyAndAttack (D.C, D.R3) I.Hit,
    D.ReplyAndAttack (D.B, D.R2) I.Miss
  ]

someGameI :: I.Moves
someGameI = I.Moves ["C","3"] (Just I.Hit) $ Just (
  I.Moves ["B","2"] (Just I.Miss) $ Just (
    I.Moves ["A","1"] Nothing Nothing)
  )

common :: TestTree
common = testGroup "Smoke test" [
  testCase "map domain to interface" $ 
    D.toNestedMoves someGame @?= someGameI,
  testCase "renders json" $
    Aeson.encode someGameI @?= "{\"coord\":[\"C\",\"3\"],\"result\":\"HIT\",\"prev\":{\"coord\":[\"B\",\"2\"],\"result\":\"MISS\",\"prev\":{\"coord\":[\"A\",\"1\"],\"result\":null,\"prev\":null}}}",
  testCase "renders bencode" $
    Ben.encode someGameI @?= "d5:coordl1:C1:3e4:prevd5:coordl1:B1:2e4:prevd5:coordl1:A1:1ee6:result4:MISSe6:result3:HITe"]

jsonNoMaps :: TestTree
jsonNoMaps = testGroup "Eliminate maps" [
  testCase "root" $ 
    I.withOutMaps (A.object [("a", A.emptyArray), ("b", A.String "b")]) @?=
      jarray [A.String "a", A.emptyArray, A.String "b", A.String "b"],
  testCase "nested" $
    I.withOutMaps (jarray [A.object [("b", A.String "b")], A.String "a"]) @?=
      jarray [jarray [A.String "b", A.String "b"], A.String "a"]]

jsonNoLists :: TestTree
jsonNoLists = testGroup "Eliminate lists" [
  testCase "root" $ 
    I.withOutLists (jarray [A.emptyArray, A.String "b"]) @?=
      A.object [("1", A.object []), ("2", A.String "b")],
  testCase "nested" $
    I.withOutLists (A.object [("b", A.String "b"), ("a", jarray[A.String "a", A.String "b"])]) @?=
      A.object [("b", A.String "b"), ("a", A.object[("1", A.String "a"), ("2", A.String "b")])]]


jarray :: [A.Value] -> A.Value
jarray = A.Array . V.fromList
