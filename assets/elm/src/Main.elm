module Main exposing (..)

import Html as Html exposing (..)

type Msg
 = OnLocationChange

type alias Model =
  { greeting : String }

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

initialModel : Model
initialModel =
  { greeting = "Hello and welcome to the PokerEx client app, written in Elm." }

view : Model -> Html Msg
view model =
  div [] [ text model.greeting ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    _ -> ( model, Cmd.none )

main : Program Never Model Msg
main =
  Html.program
      { init = init
      , view = view
      , subscriptions = (\model -> Sub.none)
      , update = update
      }
