import Html exposing (Html, div, span, h2, h3, input, button, text, a)
import Html.Attributes exposing (id, class, placeholder, value, type_, href)
import Html.Events exposing (onInput, onFocus, onBlur)
import Http
import Regex exposing (Regex, regex)

import KeyValue


main : Program Flags Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Flags =
  { request: Maybe String
  , token: Maybe String
  }

-- MODEL

type alias Model =
  { api: String
  , token: String
  , focusToken: Bool
  , path: KeyValue.Model
  , feature: String
  , params: KeyValue.Model
  }


urlRegex : Regex
urlRegex =
  regex "^([^?]*/v[0-9]+/)([^?]*)\\??([^#]*)?(#.*)*$"


init : Flags -> (Model, Cmd Msg)
init flags =
  let
    submatches =
      Maybe.withDefault "" flags.request
        |> Regex.find (Regex.AtMost 1) urlRegex
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.withDefault []

    api =
      submatches
        |> nth 0
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ""

    pathAndFeature =
      submatches
        |> nth 1
        |> Maybe.withDefault Nothing
        |> Maybe.map (String.split "/")
        |> Maybe.withDefault []
        |> List.filter (String.isEmpty >> not)
        |> List.map (\s -> Http.decodeUri s |> Maybe.withDefault s)

    zipPath l =
      case l of
          k :: v :: q -> (k, v) :: zipPath q
          _ -> []
    path =
      zipPath pathAndFeature

    feature =
      if List.length pathAndFeature % 2 == 0 then
        Nothing
      else
        List.reverse pathAndFeature |> List.head

    zipParam l =
      case l of
          [] -> ("", "")
          t :: q -> (t, String.join "=" q)
    params =
      submatches
        |> nth 2
        |> Maybe.withDefault Nothing
        |> Maybe.map (String.split "&")
        |> Maybe.withDefault []
        |> List.filter (String.isEmpty >> not)
        |> List.map (\s -> Http.decodeUri s |> Maybe.withDefault s)
        |> List.map (Regex.split (Regex.AtMost 1) (regex "="))
        |> List.map zipParam
  in
      { api = api
      , token = Maybe.withDefault "" flags.token
      , focusToken = False
      , path = KeyValue.model path
      , feature = Maybe.withDefault "" feature
      , params = KeyValue.model params
      } ! []


-- UPDATE

type Msg
  = InputApi String
  | InputToken String
  | FocusToken Bool
  | Path KeyValue.Msg
  | InputFeature String
  | Params KeyValue.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      InputApi s ->
        { model | api = s } ! []

      InputToken s ->
        { model | token = s } ! []

      FocusToken isFocus ->
        { model | focusToken = isFocus } ! []
      
      Path msg ->
        { model | path = KeyValue.update msg model.path } ! []

      InputFeature s ->
        { model | feature = s } ! []

      Params msg ->
        { model | params = KeyValue.update msg model.params } ! []
 


-- VIEW

view : Model -> Html Msg
view model =
  let
    path =
      List.map (Html.map Path) (KeyValue.view model.path)

    feature =
      div [ id "feature", class "inputDiv" ]
        [ input
            [ onInput InputFeature
            , value model.feature
            , placeholder "empty feature"
            , Html.Attributes.attribute "onClick" "this.select();"
            ]
            []
        ]

    params =
      List.map (Html.map Params) (KeyValue.view model.params)

    url =
      model.api
        ++ KeyValue.encodeUri "/" "/" model.path
        ++ "/"
        ++ Http.encodeUri model.feature
        ++ "?"
        ++ KeyValue.encodeUri "=" "&" model.params

    prettyUrl =
      [ span [ class "api" ] [ text model.api ]
      , span [ class "path" ] [ text (KeyValue.encodeUri "/" "/" model.path ++ "/") ]
      , span [ class "feature" ] [ text (Http.encodeUri model.feature) ]
      , span [ class "parameters" ] [ text (KeyValue.encodeUri "=" "&" model.params) ]
      ]
  in
      div []
        [ h2 [] [ text "Create a request" ]
        , h3 [] [ text "Fill out credential info" ]
        , div [ id "credentials" ]
            [ div [ class "inputDiv" ]
                [ span [ class "key" ] [ text "API" ]
                , input
                    [ onInput InputApi
                    , value model.api
                    , placeholder "API"
                    , Html.Attributes.attribute "onClick" "this.select();"
                    ]
                    []
                ]
            , div [ class "inputDiv" ]
                [ span [ class "key" ] [ text "Token" ]
                , input
                    [ onInput InputToken
                    , value model.token
                    , placeholder "Token"
                    , type_ <| if model.focusToken then "text" else "password"
                    -- Doesn't work because type is changed after onClick :/
                    --, Html.Attributes.attribute "onClick" "this.select();"
                    , onFocus (FocusToken True)
                    , onBlur (FocusToken False)
                    ]
                    []
                ]
            ]
        , h3 [] [ text "Build your path" ]
        , div [ id "path" ] <| path ++ [ feature ]
        , h3 [] [ text "Add parameters" ]
        , div [ id "parameters" ] params
        , h3 [] [ text "Send the request" ]
        , div [ id "urlDiv", class "inputDiv" ]
          [ span [ class "key" ] [ text "URL" ]
          , span [ id "requestUrl" ]
              [ a [ href url ] prettyUrl ]
          ]
        , div [ id "submitDiv" ] [ button [] [ text "Submit" ] ]
        ]


nth : Int -> List a -> Maybe a
nth i l =
  case (i, l) of
      (_, []) -> Nothing
      (0, t :: _) -> Just t
      (i, _ :: q) -> nth (i - 1) q
