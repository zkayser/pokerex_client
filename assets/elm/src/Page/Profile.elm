module Page.Profile exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Html as Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events

type alias Model = {
  player : Player
}

type Msg
  = NoOp

-- INITIALIZATION
initialModel : Player -> Model
initialModel player =
  { player = player }

-- VIEW
view : Session -> Model -> Html Msg
view session model =
  div []
    [ h1 [ class "teal-text" ] [ text <| "Hello, " ++ Player.usernameToString model.player.username ] ]

-- SUBSCRIPTIONS
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Sub.none