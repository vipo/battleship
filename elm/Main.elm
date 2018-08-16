import Html exposing (Html, Attribute, button, div, span, text, h2, table, tr,
  td, caption, textarea, label, input, fieldset, section)
import Html.Attributes exposing(style, readonly, cols, rows, type_, checked)
import Html.Events exposing (onClick)
import Random

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
init = (Model emptyBoard emptyBoard "" Classical Json 0, Cmd.none)

-- UPDATE

type Msg = SwitchMsgType MsgType |
  SwitchGameType GameType |
  GenGame |
  NewSeed Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SwitchMsgType newMsgType ->
      ({ model | msgType = newMsgType }, Cmd.none)
    SwitchGameType newGameType ->
      ({ model | gameType = newGameType }, Cmd.none)
    GenGame ->
      (model, Random.generate NewSeed (Random.int 1 32000))
    NewSeed newSeed->
      ({ model | seed = newSeed }, Cmd.none)


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