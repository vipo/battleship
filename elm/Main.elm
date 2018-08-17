import Html exposing (Html, Attribute, button, div, span, text, h2, table, tr,
  td, caption, textarea, label, input, fieldset, section)
import Html.Attributes exposing(style, readonly, cols, rows, type_, checked)
import Html.Events exposing (onClick)
import Random
import Http

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

type alias Board = List (List CellState)

type alias Model = {
    boardA : Board
  , boardB : Board
  , rawMessage : String
  , gameType : GameType
  , msgType : MsgType
  , seed : Int
}

emptyBoard : Board
emptyBoard = List.repeat 10 (List.repeat 10 Empty)

init : (Model, Cmd Msg)
init = (Model emptyBoard emptyBoard "" Classical Json 0, genSeed)

-- UPDATE

type Msg = SwitchMsgType MsgType |
  SwitchGameType GameType |
  GenGame |
  NewSeed Int |
  SetRaw (Result Http.Error String)

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
        (newModel, Http.send SetRaw (getRawContent newModel))
    SetRaw result ->
      case result of
        Ok m -> ({ model | rawMessage = m}, Cmd.none)
        Err m -> ({ model | rawMessage = toString m}, Cmd.none)

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

getRawContent : Model -> Http.Request String
getRawContent model =
  Http.request { 
      method = "GET"
    , headers = [Http.header "Accept" (msgTypeToContentType model.msgType)]
    , url = "/game/" ++ gameTypeToUrl model.gameType ++ "/arbitrary?seed=" ++ toString (model.seed)
    , body = Http.emptyBody
    , expect = Http.expectString
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

toTableRow : (String, List CellState) -> List (Html Msg)
toTableRow (n, r) =
  [ tr [tableStyle]
    (
    td [tableStyle] [text n] :: List.map (\h -> td [tableStyle][renderState h]) r
    )
  ]

tableStyle : Attribute Msg
tableStyle =
  style
    [ ("border", "1px solid black")
    , ("border-collapse", "collapse")
    , ("font-family", "monospace")
    ]

renderTable : String -> Board -> Html Msg
renderTable cap b =
  table [tableStyle] (
    (caption [] [text cap]) ::
    (List.map (\h -> td [tableStyle][text h]) [" ","A","B","C","D","E","F","G","H","I","J"] |> tr []) ::
    (b 
      |> List.map2 (,) ["1","2","3","4","5","6","7","8","9","10"]
      |> List.concatMap toTableRow
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