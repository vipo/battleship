import Html exposing (Html, Attribute, button, div, span, text, h2, table, tr,
  td, caption, textarea, label, input, fieldset, section)
import Html.Attributes exposing(style, readonly, cols, rows, type_, checked)
import Html.Events exposing (onClick)
import Random
import Http

import Array

import Api exposing (Moves, decodeMoves)

main : Program Never Model Msg
main = Html.program {
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type GameType = Classical | Tetris | TShapes

type MsgType = Json |      JsonNoLists |      JsonNoMaps |
          Bencoding | BencodingNoLists | BencodingNoMaps

type CellState = Empty | Hit Int | Miss Int

type alias Board = Array.Array (Array.Array CellState) -- col (row)

type alias Model = {
    boardA : Board
  , boardB : Board
  , rawMessage : String
  , gameType : GameType
  , msgType : MsgType
  , seed : Int
}

emptyBoard : Board
emptyBoard = Array.repeat 10 (Array.repeat 10 Empty)

init : (Model, Cmd Msg)
init = (Model emptyBoard emptyBoard "" Classical Json 0, genSeed)

-- UPDATE

type Msg = SwitchMsgType MsgType |
  SwitchGameType GameType |
  GenGame |
  NewSeed Int |
  SetRaw (Result Http.Error String) |
  SetBoards (Result Http.Error Api.Moves)

genSeed : Cmd Msg
genSeed = Random.generate NewSeed (Random.int 1 32000)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SwitchMsgType newMsgType ->
      ({ model | msgType = newMsgType }, Cmd.none)
    SwitchGameType newGameType ->
      ({ model | gameType = newGameType }, Cmd.none)
    GenGame ->
      (model, genSeed)
    NewSeed newSeed ->
      let
        newModel = { model | seed = newSeed }
      in
        (newModel, Cmd.batch
          [
            Http.send SetRaw    (getRawContent newModel.msgType newModel.gameType newModel.seed Http.expectString),
            Http.send SetBoards (getRawContent Json             newModel.gameType newModel.seed (Http.expectJson Api.decodeMoves))
          ]
        )
    SetRaw result ->
      case result of
        Ok m -> ({ model | rawMessage = m}, Cmd.none)
        Err m -> ({ model | rawMessage = toString m}, Cmd.none)
    SetBoards result ->
      (model, Cmd.none)

msgTypeToContentType : MsgType -> String
msgTypeToContentType msgType =
  case msgType of
    Json ->        "application/json"
    JsonNoLists -> "application/json+nolists"
    JsonNoMaps ->  "application/json+nomaps"
    Bencoding ->        "application/bencoding"
    BencodingNoLists -> "application/bencoding+nolists"
    BencodingNoMaps ->  "application/bencoding+nomaps"

gameTypeToUrl : GameType -> String
gameTypeToUrl gameType =
  case gameType of
    Classical -> "classical"
    Tetris ->    "tetris"
    TShapes ->   "t-shapes"

getRawContent : MsgType -> GameType -> Int -> Http.Expect a -> Http.Request a
getRawContent msgType gameType seed expect =
  Http.request { 
      method = "GET"
    , headers = [Http.header "Accept" (msgTypeToContentType msgType)]
    , url = "/game/" ++ gameTypeToUrl gameType ++ "/arbitrary?seed=" ++ toString seed
    , body = Http.emptyBody
    , expect = expect
    , timeout = Nothing
    , withCredentials = False
    }

-- VIEW

renderState : CellState -> Html Msg
renderState s =
  case s of
    Empty -> text " "
    Hit n -> span [style[("color","red")]] [text(toString(n))]
    Miss n -> span [style[("color","blue")]] [text(toString(n))]

tableStyle : Attribute Msg
tableStyle =
  style
    [ ("border", "1px solid black")
    , ("border-collapse", "collapse")
    , ("font-family", "monospace")
    ]

safeGet : Board -> Int -> Int -> CellState
safeGet b col row =
  (Array.get col b) |> Maybe.andThen (Array.get row) |> Maybe.withDefault Empty

toTableRow : Board -> Int -> List (Html Msg)
toTableRow b row =
  [ tr [tableStyle]
    (
    td [tableStyle] [row+1 |> toString |> text] :: List.map (\col -> td [tableStyle][renderState (safeGet b col row)]) (List.range 0 9)
    )
  ]

renderTable : String -> Board -> Html Msg
renderTable cap b =
  table [tableStyle] (
    (caption [] [text cap]) ::
    (List.map (\h -> td [tableStyle][text h]) [" ","A","B","C","D","E","F","G","H","I","J"] |> tr []) ::
    (
      List.range 0 9 |> List.concatMap (toTableRow b)
    )
  )

view : Model -> Html Msg
view model =
  div []
    [
      h2 [] [text "Boards"]
    , div [style [("float", "left")]]
      [
        renderTable "Player's A board" model.boardA
      ]
    , div [style [("float", "left")]]
      [
        renderTable "Player's B board" model.boardB
      ]
    , div [style [("clear", "both")]] [
        h2 [] [text "Message"]
      , textarea [readonly True, cols 128, rows 8] [text model.rawMessage]
      , fieldset []
        [ msgTypeRadio model.msgType Json        "Json"
        , msgTypeRadio model.msgType JsonNoLists "Json w/o lists"
        , msgTypeRadio model.msgType JsonNoMaps  "Json w/o maps"
        , msgTypeRadio model.msgType Bencoding        "Bencoding"
        , msgTypeRadio model.msgType BencodingNoLists "Bencoding w/o lists"
        , msgTypeRadio model.msgType BencodingNoMaps  "Bencoding w/o maps"
        ]
      , fieldset []
        [ gameTypeRadio model.gameType Classical "Classical"
        , gameTypeRadio model.gameType Tetris    "Tetris"
        , gameTypeRadio model.gameType TShapes   "T-shapes"
        ]
      , div[] [
        span [] [text ("Seed: " ++ toString model.seed)]
        ]
      , button [ onClick GenGame ] [ text "Generate" ]
      ]
    ]

msgTypeRadio : MsgType -> MsgType -> String -> Html Msg
msgTypeRadio state switchState name =
  label []
    [ input [ type_ "radio", onClick (SwitchMsgType switchState), checked (state == switchState)] []
    , text name
    ]

gameTypeRadio : GameType -> GameType -> String -> Html Msg
gameTypeRadio state switchState name =
  label []
    [ input [ type_ "radio", onClick (SwitchGameType switchState), checked (state == switchState)] []
    , text name
    ]