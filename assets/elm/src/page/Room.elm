module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)

-- Boiler Plate

type Msg
  = DoNothing

type ExternalMsg
  = NoOp

-- This may eventually contain a lot of data (players, chips, table state, etc.)
type alias Model =
  { room : String 
  , players : List Player
  }

initialModel : Model
initialModel =
  { room = "Elm development"
  , players = []
  }

view : Session -> Model -> Html Msg
view session model =
  div [ class "room-container" ] 
    [ text "This will eventually be the room..." ]

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    DoNothing -> Debug.log "Got DoNothing message from room" ( (model, Cmd.none ), NoOp )