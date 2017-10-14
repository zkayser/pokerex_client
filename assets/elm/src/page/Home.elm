module Page.Home exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

-- MODEL --

type alias Model =
  { greeting : String }

initialModel : Model
initialModel =
  { greeting = "Welcome to the Elm PokerEx Client"}

-- VIEW --

view : Session -> Model -> Html msg
view session model =
  div [style [ ( "text-align", "center" ) ] ]
    [ div [] [ text model.greeting ] ]

-- UPDATE --
type Msg
  = DoNothing -- Remove when actually implementing

type ExternalMsg
  = NoOp

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
  case msg of
    _ ->
      ( ( model, Cmd.none ), NoOp )
