module Page.Home exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (style, class)

-- MODEL --

type alias Model =
  { greeting : String }

initialModel : Model
initialModel =
  { greeting = "Welcome to PokerEx!"}

-- VIEW --

view : Session -> Model -> Html msg
view session model =
  div [ class "hero valign-wrapper" ]
      [ h1 [ class "welcome", style [ ("text-align", "center")] ] 
        [ text model.greeting ] 
      ]

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
