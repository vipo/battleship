module Api exposing (..)

import Json.Decode exposing (Decoder, decodeString, andThen, string, int, list, maybe, succeed, fail, lazy, map3, field)

type Moves = Moves
  { coord : Maybe (Int, Int) -- (col, row), zero-based
  , result : Maybe (MoveResult)
  , prev : Maybe (Moves)
  }

type MoveResult
  = Miss
  | Hit

toCol : String -> Result String Int
toCol s =
  case s of
    "A" -> Ok 0
    "B" -> Ok 1
    "C" -> Ok 2
    "D" -> Ok 3 
    "E" -> Ok 4
    "F" -> Ok 5
    "G" -> Ok 6
    "H" -> Ok 7
    "I" -> Ok 8
    "J" -> Ok 9 
    a -> Err ("Illegal column: " ++ a)

toRow : String -> Result String Int
toRow s =
  let
    mapping i = if i > 0 && i < 11 then Ok (i-1) else Err ("Illegal row: " ++ toString(i))
  in
    String.toInt s |> Result.andThen mapping

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

decodeCoord : Decoder (Maybe (Int, Int))
decodeCoord =
  let
    mapping s =
      case s of
        Just ([c, r]) ->
          case (toCol c |> Result.andThen (\col -> Result.map (\v -> (col, v)) (toRow r))) of
            Ok t -> succeed (Just t)
            Err err -> fail ("Coord decoder: " ++ err)
        Just ([]) -> succeed Nothing
        Nothing -> succeed Nothing
        a -> fail ("Illegal coordinates:" ++ toString(a))
  in
    maybe (list string) |> andThen mapping

decodeMoves : Decoder Moves
decodeMoves =
  let
    cons coord result prev = Moves {coord = coord, result = result, prev = prev}
  in
    map3 cons
      (field "coord" decodeCoord)
      (field "result" (maybe decodeMoveResult))
      (field "prev" (maybe (lazy(\_ -> decodeMoves))))