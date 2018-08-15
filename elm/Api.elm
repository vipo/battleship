module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Moves =
    { coord : List (String)
    , result : Maybe (MoveResult)
    , prev : Maybe (Moves)
    }

decodeMoves : Decoder Moves
decodeMoves =
    decode Moves
        |> required "coord" (list string)
        |> required "result" (maybe decodeMoveResult)
        |> required "prev" (maybe decodeMoves)

getGameByVariationArbitrary : GameVariation -> Maybe (Int) -> Http.Request (Moves)
getGameByVariationArbitrary capture_variation query_seed =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_seed
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "seed=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ ""
                    , "game"
                    , capture_variation |> toString |> Http.encodeUri
                    , "arbitrary"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeMoves
            , timeout =
                Nothing
            , withCredentials =
                False
            }