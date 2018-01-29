module Page.Rooms exposing (..)

import Data.Session as Session exposing (Session)
import Data.AuthToken as AuthToken
import Data.Configuration exposing (Configuration)
import Widgets.Pagination as Pagination exposing (paginate)
import Route
import Html as Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
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
  , socketUrl : String
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
initialModel : Configuration -> Model
initialModel envConfig =
  { rooms = []
  , page = 1
  , totalPages = 0
  , channelSubscriptions = [ lobbyChannel ]
  , socketUrl = envConfig.socketUrl
  }

-- View
view : Session -> Model -> Html Msg
view session model =
  div [ class "lobby-container" ]
    [ div [ class "lobby-title" ]
      [ h1 [ class "text-center teal-text" ] [ text "Lobby" ] ]
    , (paginate model paginationConfig )
    , ul [ class "rooms-list collection" ]
      (List.map viewRoom model.rooms)
    , (paginate model paginationConfig )
    ]

viewRoom : RoomInfo -> Html Msg
viewRoom roomInfo =
  li [ class <| "collection-item " ++ (toString roomInfo.status) ++ " room-info-item" ]
    [ div [ class "room-list-title" ]
      [ span [ class "teal-text" ]
        [ a [ Route.href (Route.Room "public" roomInfo.room)] [ text roomInfo.room ] ]
      ]
    , div [ class "room-list-status" ]
      [ p
        [ class "room-list-player-count"]
        [ text <| "Active Players: " ++ (toString roomInfo.playerCount) ]
      , p
        [ class <| "status " ++ (toString roomInfo.status)]
        [text <| "Status: " ++ (statusToString roomInfo.status)]
      ]
    ]

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
  let
    pushMessage =
      case text of
        "keyboard_arrow_right" ->
          if onLastPage model then Cmd.none else getPage (toString <| model.page + 1) model.socketUrl
        "keyboard_arrow_left" ->
          if onFirstPage model then Cmd.none else getPage (toString <| model.page - 1) model.socketUrl
        "First" -> getPage "1" model.socketUrl
        "Last" -> getPage (toString model.totalPages) model.socketUrl
        _ -> getPage text model.socketUrl
  in
  ( ( model, pushMessage), NoOp )

-- Socket config
socket : Session -> String -> Socket Msg
socket session socketUrl =
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

-- Push messages
getPage : String -> String -> Cmd Msg
getPage page_num socketUrl =
  let
    push =
      Push.init ("lobby:lobby") "get_page"
        |> Push.withPayload ( Encode.object [ ( "page_num", (Encode.string page_num) ) ] )
  in
  Phoenix.push socketUrl push

-- Decoders
decoder : Model -> Decoder Model
decoder model =
  decode Model
    |> required "rooms" (Decode.list roomInfoDecoder)
    |> required "page" Decode.int
    |> required "total_pages" Decode.int
    |> hardcoded model.channelSubscriptions
    |> hardcoded model.socketUrl

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
      [ Phoenix.connect (socket session model.socketUrl) model.channelSubscriptions ]
  in
  Sub.batch phoenixSubscriptions

-- Widget Config
paginationConfig : Pagination.Config Msg
paginationConfig =
  { onClickMsg = PaginationItemClicked, linksToShow = 5 }

-- Helpers
statusToString : RoomStatus -> String
statusToString status =
  case status of
    WaitingForPlayers -> "Waiting for Players"
    _ -> toString status

onLastPage : Model -> Bool
onLastPage model =
  model.page == model.totalPages

onFirstPage : Model -> Bool
onFirstPage model =
  model.page == 1