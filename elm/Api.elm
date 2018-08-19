module Api exposing (..)

import Json.Decode exposing (Decoder, decodeString, andThen, string, int, list, maybe, succeed, fail, lazy, map3, field)
import Json.Decode.Pipeline exposing (decode, required)

type Moves = Moves
  { coord : (Int, Int) -- (col, row), zero-based
  , result : Maybe (MoveResult)
  , prev : Maybe (Moves)
  }

type MoveResult
  = Miss
  | Hit

toCol : Decoder Int
toCol =
  let
    mapping s =
      case s of
        "A" -> succeed 0
        "B" -> succeed 1
        "C" -> succeed 2
        "D" -> succeed 3 
        "E" -> succeed 4
        "F" -> succeed 5
        "G" -> succeed 6
        "H" -> succeed 7
        "I" -> succeed 8
        "J" -> succeed 9 
        a -> fail ("Illegal column: " ++ a)
  in
    string |> andThen mapping

toRow : Decoder Int
toRow =
  let
    mapping i = if i > 0 && i < 11 then succeed (i-1) else fail ("Illegal row: " ++ toString(i))
  in
    int |> andThen mapping

decodeMoveResult : Decoder MoveResult
decodeMoveResult =
  let
    mapping s =
      case s of
        "MISS" -> succeed Miss
        "HIT" -> succeed Hit
        a -> fail ("Unknown move result: " ++ a)
  in
    string |> andThen mapping

decodeCoord : Decoder (Int, Int)
decodeCoord =
  let
    mapping s =
      case s of
        [c, r] ->
          case (decodeString toCol c |> Result.andThen (\col -> Result.map (\v -> (col, v)) (decodeString toRow r))) of
            Ok t -> succeed t
            Err err -> fail err
        a -> fail ("Illegal coordinates:" ++ toString(a))
  in
    list string |> andThen mapping

decodeMoves : Decoder Moves
decodeMoves =
  let
    cons coord result prev = Moves {coord = coord, result = result, prev = prev}
  in
    decode cons
      |> required "coord" decodeCoord
      |> required "result" (maybe decodeMoveResult)
      |> required "prev" (maybe (lazy(\_ -> decodeMoves)))