module Page.Rooms exposing (..)

import Data.Session as Session exposing (Session)
import Data.AuthToken as AuthToken
import Html as Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Page.Room.SocketConfig exposing (socketUrl)
import Phoenix.Push as Push exposing (Push)
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix

type alias RoomInfo = { room : String, playerCount : Int, status : RoomStatus }
type alias RoomInfoList = List RoomInfo

type alias Model =
  { rooms : RoomInfoList
  , page : Int
  , totalPages : Int
  , channelSubscriptions : List (Channel Msg)
  }

type RoomStatus
  = Full
  | Empty
  | WaitingForPlayers
  | Active

type Msg
  = JoinedLobby
  | JoinFailed Decode.Value
  | UpdatePlayerCount Decode.Value
  | UpdateRooms Decode.Value
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

-- Initialization
initialModel : Model
initialModel =
  { rooms = []
  , page = 1
  , totalPages = 0
  , channelSubscriptions = [ lobbyChannel ]
  }

-- View
view : Session -> Model -> Html Msg
view session model =
  h1 [ class "teal-text" ] [ text "Lobby" ]

-- Update
update : Msg -> Model -> ( ( Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    JoinedLobby ->               ( ( model, Cmd.none ), NoOp )
    JoinFailed payload ->        handleJoinFailed model payload
    UpdatePlayerCount payload -> handleUpdatePlayerCount model payload
    UpdateRooms payload ->       handleUpdateRooms model payload
    SocketOpened ->              ( ( model, Cmd.none ), NoOp )
    SocketClosed ->              ( ( model, Cmd.none ), NoOp )
    SocketClosedAbnormally ->    ( ( model, Cmd.none ), NoOp )

handleJoinFailed : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleJoinFailed model payload =
  Debug.log ("Failed to join lobby channel with payload: " ++ (toString payload))
  ( ( model, Cmd.none), NoOp )

handleUpdatePlayerCount : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdatePlayerCount model payload =
  let
    newRooms =
      case Decode.decodeValue roomInfoDecoder payload of
        Ok newRoomInfo ->
          List.map (\info -> if info.room == newRoomInfo.room then newRoomInfo else info) model.rooms
        Err _ -> model.rooms
  in
  Debug.log "UPDATING ROOMS...."
  ( ( { model | rooms = newRooms }, Cmd.none), NoOp )

handleUpdateRooms : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateRooms model payload =
  let
    newModel =
      case Decode.decodeValue (decoder model) payload of
        (Ok newModel) -> newModel
        (Err _) -> model
  in
  ( ( newModel, Cmd.none ), NoOp )


-- Socket config
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

-- Channel config
lobbyChannel : Channel Msg
lobbyChannel =
  Channel.init ("lobby:lobby")
    |> Channel.withPayload (Encode.object [] )
    |> Channel.onJoin (\_ -> JoinedLobby)
    |> Channel.onJoinError (\json -> JoinFailed json)
    |> Channel.on "update_player_count" (\payload -> UpdatePlayerCount payload)
    |> Channel.on "rooms" (\payload -> UpdateRooms payload)
    |> Channel.withDebug

-- Decoders
decoder : Model -> Decoder Model
decoder model =
  decode Model
    |> required "rooms" (Decode.list roomInfoDecoder)
    |> required "page" Decode.int
    |> required "total_pages" Decode.int
    |> hardcoded model.channelSubscriptions

roomInfoDecoder : Decoder RoomInfo
roomInfoDecoder =
  decode RoomInfo
    |> required "room" Decode.string
    |> required "player_count" Decode.int
    |> required "player_count" (Decode.int |> Decode.andThen (\int -> numToStatus int))

numToStatus : Int -> Decoder RoomStatus
numToStatus num =
  case num of
    0 -> Decode.succeed Empty
    1 -> Decode.succeed WaitingForPlayers
    7 -> Decode.succeed Full
    _ -> Decode.succeed Active

-- Subscriptions
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions ]
  in
  Sub.batch phoenixSubscriptions
