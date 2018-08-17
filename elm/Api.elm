module Api exposing (..)

import Json.Decode exposing (Decoder, andThen, string, list, maybe, succeed, fail, lazy)
import Json.Decode.Pipeline exposing (decode, required)

type Moves = Moves
  { coord : List (String)
  , result : Maybe (MoveResult)
  , prev : Maybe (Moves)
  }

type MoveResult
  = Miss
  | Hit

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

decodeMoves : Decoder Moves
decodeMoves =
  decode Moves
    |> required "coord" (list string)
    |> required "result" (maybe decodeMoveResult)
    |> required "prev" (maybe (lazy(\_ -> decodeMoves)))