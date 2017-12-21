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
  | PaginationItemClicked String
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
  div [ class "lobby-container" ]
    [ div [ class "lobby-title" ]
      [ h1 [ class "text-center teal-text" ] [ text "Lobby" ] ]
    , ul [ class "pagination-list" ]
      (viewPagination model.totalPages)
    , ul [ class "rooms-list collection" ]
      (List.map viewRoom model.rooms)
    ]

viewRoom : RoomInfo -> Html Msg
viewRoom roomInfo =
  li [ class <| "collection-item " ++ (toString roomInfo.status) ++ " room-info-item" ]
    [ div [ class "room-list-title" ]
      [ span [ class "teal-text" ] [ text roomInfo.room ] ]
    , div [ class "room-list-status" ]
      [ span
        [ class "room-list-player-count"]
        [ text <| "Active Players: " ++ (toString roomInfo.playerCount) ]
      , span
        [ class <| "status " ++ (toString roomInfo.status)]
        [text <| "Status: " ++ (statusToString roomInfo.status)]
      ]
    ]

viewPagination : Int -> List (Html Msg)
viewPagination pageCount =
  List.map (\text -> viewPaginationItem text) (paginationText pageCount)

viewPaginationItem : String -> Html Msg
viewPaginationItem paginationText =
  li [ class "pagination-list-item teal-text" ]
    [ a [ onClick (PaginationItemClicked paginationText) ] (viewItemText paginationText) ]

viewItemText : String -> List (Html Msg)
viewItemText paginationText =
  if List.member paginationText ["keyboard_arrow_left", "keyboard_arrow_right"] then
    [ i [ class "material-icons" ] [ text paginationText ] ]
  else
    [ text paginationText ]

paginationText : Int -> List String
paginationText pageCount =
  [ "keyboard_arrow_left" ]
  ++ (List.map (\num -> toString num) (List.range 1 pageCount))
  ++ [ "keyboard_arrow_right" ]

-- Update
update : Msg -> Model -> ( ( Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    JoinedLobby ->                ( ( model, Cmd.none ), NoOp )
    JoinFailed payload ->         ( ( model, Cmd.none ), NoOp )
    UpdatePlayerCount payload ->  handleUpdatePlayerCount model payload
    UpdateRooms payload ->        handleUpdateRooms model payload
    PaginationItemClicked text -> handlePaginationItemClicked model text
    SocketOpened ->               ( ( model, Cmd.none ), NoOp )
    SocketClosed ->               ( ( model, Cmd.none ), NoOp )
    SocketClosedAbnormally ->     ( ( model, Cmd.none ), NoOp )

handleJoinFailed : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleJoinFailed model payload =
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

handlePaginationItemClicked : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handlePaginationItemClicked model text =
  Debug.log "TODO: Implement handlePaginationItemClicked; Now performing a NoOp"
  ( ( model, Cmd.none), NoOp )


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

-- Helpers
statusToString : RoomStatus -> String
statusToString status =
  case status of
    WaitingForPlayers -> "Waiting for Players"
    _ -> toString status
