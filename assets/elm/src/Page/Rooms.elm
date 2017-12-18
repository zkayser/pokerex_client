module Page.Rooms exposing (..)

import Data.Session as Session exposing (Session)
import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onClick)

type alias RoomInfo = { name : String, activePlayers : Int, full : Bool, empty : Bool }

type alias Model = { rooms : List RoomInfo, page : Int }

type Msg
  = NoOp

-- Initialization
initialModel : Model
initialModel = { rooms = [], page = 1 }

-- View
view : Session -> Model -> Html Msg
view session model =
  h1 [ class "teal-text" ] [ text "Lobby" ]


-- Subscriptions
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Sub.none