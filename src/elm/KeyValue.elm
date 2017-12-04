module KeyValue exposing (Model, empty, model, encodeUri, Msg, update, view)

import Html exposing (Html, div, span, input, button, text, img)
import Html.Attributes exposing (id, class, type_, placeholder, value, disabled, src, alt)
import Html.Events exposing (onClick, onInput)
import Http


type alias Model =
  { keysValues: List (String, String)
  , new: String
  }


empty : Model
empty =
  { keysValues = []
  , new = ""
  }


model : List (String, String) -> Model
model kvs =
  { keysValues = kvs
  , new = ""
  }


encodeUri : String -> String -> Model -> String
encodeUri kvSep eltSep model =
  model.keysValues
    |> List.map (\(k, v) -> Http.encodeUri k ++ kvSep ++ Http.encodeUri v)
    |> String.join eltSep

type Msg
  = Add
  | InputNewKey String
  | InputValue Int String
  | Delete Int


update : Msg -> Model -> Model
update msg model =
  case msg of
      Add ->
        { model | keysValues = model.keysValues ++ [(model.new, "")], new = "" }

      InputNewKey s ->
        { model | new = s }

      InputValue i new ->
        { model | keysValues = updateValueAt i new model.keysValues }

      Delete i ->
        { model | keysValues = remove i model.keysValues }

view : Model -> List (Html Msg)
view model =
  let
    keysValues =
      List.indexedMap makeKeyValue model.keysValues

    new =
      div [ class "inputDiv" ]
        [ input
            [ class "addInput"
            , onInput InputNewKey
            , type_ "text"
            , placeholder "type a key to add a path element"
            ]
            []
        , button
            [ class "add",
                onClick Add,
                disabled (String.isEmpty model.new)
            ]
            [ text "add" ]
        ]
  in
      keysValues ++ [ new ]


makeKeyValue : Int -> (String, String) -> Html Msg
makeKeyValue i (k, v) =
  div [ class "inputDiv" ]
    [ span [ class "key" ] [ text k ]
    , input [ class "value", onInput (InputValue i), value v ] []
    , button [ onClick (Delete i) ] [ img [ src "img/delete.svg", alt "delete" ] [] ]
    ]


remove : Int -> List a -> List a
remove i l =
  List.take i l ++ List.drop (i + 1) l


updateValueAt : Int -> String -> List (String, String) -> List (String, String)
updateValueAt i new l =
  List.indexedMap (\cur (k, v) -> if cur == i then (k, new) else (k, v)) l
