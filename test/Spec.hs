{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Domain as D
import           Interface

import qualified Data.List as L

import qualified Data.Aeson.Types as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.BEncode as Ben
import qualified Data.BEncode.BDict as BDict
import qualified Data.BEncode.Types as BTypes
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Data.String.Conversions

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BattleShip Specification" [common, jsonLike, bencode, json]

jsonLike :: TestTree
jsonLike = testGroup "JsonLike" [jsonLikeNoMaps, jsonLikeNoLists]

finishedGame :: D.Game
finishedGame = D.Game
  (D.A, D.R1)
  [D.LastReply Hit,
    D.ReplyAndAttack (D.C, D.R3) Hit,
    D.ReplyAndAttack (D.B, D.R2) Miss
  ]

someGame :: D.Game
someGame = D.Game
  (D.A, D.R1)
  [D.ReplyAndAttack (D.C, D.R3) Hit,
    D.ReplyAndAttack (D.B, D.R2) Miss
  ]

finishedGameI :: Moves
finishedGameI = Moves [] (Just Hit) $ Just someGameI

someGameI :: Moves
someGameI = Moves ["C","3"] (Just Hit) $ Just (
  Moves ["B","2"] (Just Miss) $ Just (
    Moves ["A","1"] Nothing Nothing)
  )

someGameJson :: BSL.ByteString
someGameJson = "{\"coord\":[\"C\",\"3\"],\"result\":\"HIT\",\"prev\":{\"coord\":[\"B\",\"2\"],\"result\":\"MISS\",\"prev\":{\"coord\":[\"A\",\"1\"]}}}"

someGameBencoding :: BSL.ByteString
someGameBencoding = "d5:coordl1:C1:3e4:prevd5:coordl1:B1:2e4:prevd5:coordl1:A1:1ee6:result4:MISSe6:result3:HITe"

someGameBencodingValue :: BTypes.BValue
someGameBencodingValue = r 
  where
    Right r = Ben.decode $ BSL.toStrict someGameBencoding

someGameJsonValue :: A.Value
someGameJsonValue = r
  where 
    Right r = Aeson.eitherDecode' someGameJson

someGameJL :: JsonLikeValue
someGameJL = JLMap [
    ("coord",  JLArray [JLString "C", JLString "3"]),
    ("prev",   JLMap [
      ("coord",  JLArray [JLString "B", JLString "2"]),
      ("prev",   JLMap [
        ("coord", JLArray [JLString "A", JLString "1"])
      ]),
      ("result", JLString "MISS")
    ]),
    ("result", JLString "HIT")
  ]

common :: TestTree
common = testGroup "Smoke test" [
  testCase "map domain to interface" $ 
    D.toNestedMoves someGame @?= someGameI,
  testCase "map interface to domain" $
    D.fromNestedMoves someGameI @?= Right someGame,
  testCase "map interface to domain (finished)" $
    D.fromNestedMoves finishedGameI @?= Right finishedGame,
  testCase "renders default json" $
    Aeson.encode someGameI @?= someGameJson,
  testCase "reads default json" $
    Aeson.decode someGameJson @?= Just someGameI
  ]

bencode :: TestTree
bencode = testGroup "Bencode" [
  testCase "to JsonLike" $ 
    Ben.fromBEncode someGameBencodingValue @?= Right someGameJL,
  testCase "from JsonLike" $
    Ben.toBEncode someGameJL @?= someGameBencodingValue
  ]
  
json :: TestTree
json = testGroup "Json" [
  testCase "to JsonLike" $ 
    sortMaps <$> Aeson.decode someGameJson @?= Just someGameJL,
  testCase "from JsonLike" $
    Aeson.decode (Aeson.encode someGameJL) @?= (Aeson.decode someGameJson :: Maybe A.Value)
  ]

sortMaps :: JsonLikeValue -> JsonLikeValue
sortMaps (JLMap l) = JLMap $ L.map (\(k, v) -> (k, sortMaps v)) (L.sortOn fst l)
sortMaps (JLArray a) = JLArray $ L.map sortMaps a
sortMaps a = a  

jsonLikeNoMaps :: TestTree
jsonLikeNoMaps = testGroup "Eliminated maps" [
  testCase "root" $ 
    withOutMaps (JLMap [("a", JLArray []), ("b", JLString "b")]) @?=
      JLArray [JLString "a", JLArray [], JLString "b", JLString "b"],
  testCase "nested" $
    withOutMaps (JLArray [JLMap [("b", JLString "b")], JLString "a"]) @?=
      JLArray [JLArray [JLString "b", JLString "b"], JLString "a"]
  ]

jsonLikeNoLists :: TestTree
jsonLikeNoLists = testGroup "Eliminated lists" [
  testCase "root" $ 
    withOutLists (JLArray [JLArray [], JLString "b"]) @?=
      JLMap [("1", JLMap []), ("2", JLString "b")],
  testCase "nested" $
    withOutLists (JLMap [("b", JLString "b"), ("a", JLArray [JLString "a", JLString "b"])]) @?=
      JLMap [("b", JLString "b"), ("a", JLMap [("1", JLString "a"), ("2", JLString "b")])]
  ]

