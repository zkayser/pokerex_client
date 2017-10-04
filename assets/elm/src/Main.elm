module Main exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (onInput, onClick)
import Json.Encode exposing (Value)
import Navigation exposing (Location)
import Route exposing (Route)

type Msg
 = SetRoute (Maybe Route)
 | SetUsername String
 | SetPassword String

-- Dummy implementation for bootstrapping purposes
setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
  case maybeRoute of
    Nothing -> ( model, Cmd.none )
    Just route -> ( model, Cmd.none )

type alias Model =
  { greeting : String
  , username : String
  , password : String
  }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
  ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  { greeting = "Hello from the Elm PokerEx client."
  , username = ""
  , password = ""
  }

view : Model -> Html Msg
view model =
  div [ style [ ("text-align", "center") ] ]
    [ text model.greeting
    , div [ style [ ("text-align", "center") ] ]
      [ input
          [ onInput SetUsername
          , placeholder "username"
          , type_ "text"
          , defaultValue model.username
          ]
          [ ]
      , input
          [ onInput SetPassword
          , placeholder "password"
          , type_ "password"
          , defaultValue model.password
          ]
          [ ]
      ]
    , div [ style [ ("text-align", "center") ]] [ text ("Username: " ++ (toString model.username)) ]
    , div [ style [ ("text-align", "center") ]] [ text ("Password: " ++ (toString model.password)) ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetUsername name ->
      ( { model | username = name}, Cmd.none )
    SetPassword pass ->
      ( { model | password = pass}, Cmd.none )
    _ -> ( model, Cmd.none )

main : Program Value Model Msg
main =
  Navigation.programWithFlags (Route.fromLocation >> SetRoute)
      { init = init
      , view = view
      , subscriptions = (\model -> Sub.none)
      , update = update
      }
