module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.AuthToken as AuthToken
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Widgets.PlayerToolbar as PlayerToolbar
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)

-- Boiler Plate

type Msg
  = NewMsg String
  | Joined
  | JoinRoom Player
  | LeaveRoom Player
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

-- This may eventually contain a lot of data (players, chips, table state, etc.)
type alias Model =
  { room : String 
  , players : List Player
  , player : Player
  , channelSubscriptions : List (Channel Msg)
  }

-- SOCKET & CHANNEL CONFIG --

socketUrl : String
socketUrl =
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
  Socket.init socketUrl
    |> Socket.withParams params
    |> Socket.onOpen (SocketOpened)
    |> Socket.onClose (\_ -> SocketClosed)
    |> Socket.onAbnormalClose (\_ -> SocketClosedAbnormally)

room : Model -> Channel Msg
room model =
  Channel.init ("players:" ++ model.room)
    |> Channel.withPayload ( Encode.object [ ("type", Encode.string "public") ] )
    |> Channel.onJoin (\_ -> Joined)
    |> Channel.withDebug


initialModel : Player -> Model
initialModel player =
  { room = "room_1" -- Should be updated to take dynamic values on load
  , players = []
  , player = player
  , channelSubscriptions = [ ] -- should be initialized to players:#{room_number}
  }

-- VIEW --

view : Session -> Model -> Html Msg
view session model =
  div [ class "room-container" ] 
    [ div [ class "table-container" ]
      (viewTableCenter :: viewPlayers session model)
    , PlayerToolbar.view (toolbarConfig model)
    ]
  
viewPlayers : Session -> Model -> List (Html Msg)
viewPlayers session model =
  List.map (viewSeat) (List.range 1 8)
  
viewTableCenter : Html Msg
viewTableCenter =
  div [ class "table-center" ]
    [ img [ id "deck", src "http://localhost:4000/images/card-back.svg.png"] [] ]

viewSeat : Int -> Html Msg
viewSeat number =
  div [ id ("seat-" ++ (toString number)), class "player-seat", style [("text-align", "center")] ]
    [ text (toString number) ]  

-- WIDGET CONFIGURATIONS --

toolbarConfig : Model -> PlayerToolbar.Config Msg
toolbarConfig model =
  let
    hasJoined =
      List.member model.player model.players
    (txt, msg) =
      if hasJoined then ("Leave", LeaveRoom model.player) else ("Join", JoinRoom model.player)
  in
  { joinLeaveMsg = msg, btnText = txt }   

-- UPDATE --

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    NewMsg message ->         ( ( model, Cmd.none), NoOp )
    Joined ->                 ( ( model, Cmd.none), NoOp )
    SocketOpened ->           ( ( model, Cmd.none), NoOp )
    SocketClosed ->           ( ( model, Cmd.none), NoOp )
    SocketClosedAbnormally -> ( ( model, Cmd.none), NoOp )
    JoinRoom player ->        handleJoinRoom player model
    LeaveRoom player ->       handleLeaveRoom player model

-- UPDATE HELPERS --

handleJoinRoom : Player -> Model -> ( (Model, Cmd Msg), ExternalMsg )
handleJoinRoom player model =
  let
    newSubscriptions =
      room model :: model.channelSubscriptions
    newModel = 
      { model | players = player :: model.players, channelSubscriptions = newSubscriptions }
  in
  ( ( newModel, Cmd.none ), NoOp )

handleLeaveRoom : Player -> Model -> ( (Model, Cmd Msg), ExternalMsg )
handleLeaveRoom player model =
  let
    filterBy = Player.usernameToString player.username
    newModel =
      { model | players = List.filter (\player -> Player.usernameToString(player.username) /= filterBy) model.players
              , channelSubscriptions = []
      }
  in
  ( (newModel, Cmd.none), NoOp )
  
-- SUBSCRIPTIONS --    

subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Phoenix.connect (socket session) model.channelSubscriptions