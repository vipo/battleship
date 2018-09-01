{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Domain as D
import qualified Interface as I
import Json as J
import Bencoding as B

import qualified Data.Aeson.Types as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.BEncode as Ben
import qualified Data.BEncode.BDict as BDict
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Data.String.Conversions

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BattleShip Specification" [common, json, bencode]

bencode :: TestTree
bencode = testGroup "Bencode" [bencodeNoMaps, bencodeNoLists]

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

someGameJson :: BSL.ByteString
someGameJson = "{\"coord\":[\"C\",\"3\"],\"result\":\"HIT\",\"prev\":{\"coord\":[\"B\",\"2\"],\"result\":\"MISS\",\"prev\":{\"coord\":[\"A\",\"1\"],\"result\":null,\"prev\":null}}}"

someGameBencoding :: BSL.ByteString
someGameBencoding = "d5:coordl1:C1:3e4:prevd5:coordl1:B1:2e4:prevd5:coordl1:A1:1ee6:result4:MISSe6:result3:HITe"

common :: TestTree
common = testGroup "Smoke test" [
  testCase "map domain to interface" $ 
    D.toNestedMoves someGame @?= someGameI,
  testCase "renders default json" $
    Aeson.encode someGameI @?= someGameJson,
  testCase "reads default json" $
    Aeson.decode someGameJson @?= Just someGameI,
  testCase "renders default bencode" $
    Ben.encode someGameI @?= someGameBencoding,
  testCase "reads default bencode" $
    Ben.decode (BSL.toStrict someGameBencoding) @?= Right someGameI
  ]

jsonNoMaps :: TestTree
jsonNoMaps = testGroup "Eliminate maps" [
  testCase "root" $ 
    J.withOutMaps (A.object [("a", A.emptyArray), ("b", A.String "b")]) @?=
      jarray [A.String "a", A.emptyArray, A.String "b", A.String "b"],
  testCase "nested" $
    J.withOutMaps (jarray [A.object [("b", A.String "b")], A.String "a"]) @?=
      jarray [jarray [A.String "b", A.String "b"], A.String "a"]
  ]

jsonNoLists :: TestTree
jsonNoLists = testGroup "Eliminated lists" [
  testCase "root" $ 
    J.withOutLists (jarray [A.emptyArray, A.String "b"]) @?=
      A.object [("1", A.object []), ("2", A.String "b")],
  testCase "nested" $
    J.withOutLists (A.object [("b", A.String "b"), ("a", jarray[A.String "a", A.String "b"])]) @?=
      A.object [("b", A.String "b"), ("a", A.object[("1", A.String "a"), ("2", A.String "b")])],
  testCase "decode" $
    J.fromWithOutLists "{\"coord\":{\"a\":\"C\",\"b\":\"3\"},\"result\":\"HIT\",\"prev\":{\"coord\":{\"a\":\"B\",\"b\":\"2\"},\"result\":\"MISS\",\"prev\":{\"coord\":{\"1\":\"A\",\"2\":\"1\"},\"result\":null,\"prev\":null}}}" @?=
      Right someGameI
  ]

bencodeNoMaps :: TestTree
bencodeNoMaps = testGroup "Eliminated maps" [
  testCase "root" $ 
    B.withOutMaps (bmap [("a", Ben.BList []), ("b", Ben.BString "b")]) @?=
      barray [Ben.BString "a", barray [], Ben.BString "b", Ben.BString "b"],
  testCase "nested" $
    B.withOutMaps (Ben.BList [bmap [("b", Ben.BString "b")], Ben.BString "a"]) @?=
      barray [barray [Ben.BString "b", Ben.BString "b"], Ben.BString "a"]
  ]

bencodeNoLists :: TestTree
bencodeNoLists = testGroup "Eliminate lists" [
  testCase "root" $ 
    B.withOutLists (barray [barray [], Ben.BString "b"]) @?=
      bmap [("1", bmap []), ("2", Ben.BString "b")],
  testCase "nested" $
    B.withOutLists (bmap [("b", Ben.BString "b"), ("a", barray [Ben.BString "a", Ben.BString "b"])]) @?=
      bmap [("b", Ben.BString "b"), ("a", bmap[("1", Ben.BString "a"), ("2", Ben.BString "b")])],
  testCase "decode" $
    B.fromWithOutLists "d5:coordd1:a1:C1:b1:3e4:prevd5:coordd1:a1:B1:b1:2e4:prevd5:coordd1:a1:A1:b1:1ee6:result4:MISSe6:result3:HITe" @?=
      Right someGameI
  ]      

jarray :: [A.Value] -> A.Value
jarray = A.Array . V.fromList

barray :: [Ben.BValue] -> Ben.BValue
barray = Ben.BList

bmap :: [(BS.ByteString, Ben.BValue)] -> Ben.BValue
bmap a = Ben.BDict $ BDict.fromAscList a