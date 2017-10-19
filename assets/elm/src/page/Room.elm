module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.AuthToken as AuthToken
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)

-- Boiler Plate

type Msg
  = DoNothing
  | NewMsg String
  | Joined
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

-- This may eventually contain a lot of data (players, chips, table state, etc.)
type alias Model =
  { room : String 
  , players : List Player
  }

lobbySocketUrl : String
lobbySocketUrl =
  "ws://localhost:3000/socket/websocket"

socket : Session -> Socket Msg
socket session =
  let
    params =
      case session.player of
        Just player ->
          let
            token = AuthToken.authTokenToString player.token
          in
          [ ( "guardian_token", token )]
        Nothing -> []
  in
  Socket.init lobbySocketUrl
    |> Socket.withParams params
    |> Socket.onOpen (SocketOpened)
    |> Socket.onClose (\_ -> SocketClosed)
    |> Socket.onAbnormalClose (\_ -> SocketClosedAbnormally)

lobby : Channel Msg
lobby =
  Channel.init "players:lobby"
    |> Channel.withPayload (Encode.object [ ("name", Encode.string "BLAH") ] )
    |> Channel.onJoin (\_ -> Joined)
    |> Channel.withDebug


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
    NewMsg message -> Debug.log ("Got a NewMsg: " ++ message) ( ( model, Cmd.none), NoOp )
    Joined -> Debug.log "Somebody joined the Room!" ( ( model, Cmd.none), NoOp)
    SocketOpened -> Debug.log "[ELM]: Socket has been opened." ( ( model, Cmd.none ), NoOp)
    SocketClosed -> Debug.log "[ELM]: Socket has closed." ( (model, Cmd.none), NoOp )
    SocketClosedAbnormally -> Debug.log "[ELM]: Abnormal close." ( ( model, Cmd.none), NoOp )


subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Phoenix.connect (socket session) [ lobby ]