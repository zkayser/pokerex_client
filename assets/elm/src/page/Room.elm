module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.AuthToken as AuthToken
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)

-- Boiler Plate

type Msg
  = DoNothing
  | NewMsg String
  | Joined -- The next two calls will be nicer if you pass in the player here.
  | JoinedRoom
  | JoinRoom (Maybe Player)
  | LeaveRoom (Maybe Player)
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

-- This may eventually contain a lot of data (players, chips, table state, etc.)
type alias Model =
  { room : String 
  , players : List Player
  , player : Maybe Player
  , channelSubscriptions : List (Channel Msg)
  }

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

lobby : Channel Msg
lobby =
  Channel.init "players:lobby"
    |> Channel.withPayload (Encode.object [ ("name", Encode.string "USERGUY") ] )
    |> Channel.onJoin (\_ -> Joined)
    |> Channel.withDebug


initialModel : Model
initialModel =
  { room = "Elm development"
  , players = []
  , player = Nothing
  }

view : Session -> Model -> Html Msg
view session model =
  div [ class "room-container" ] 
    [ div [ class "table-container" ]
      (viewTableCenter :: viewPlayers session model)
    , div [ class "controls-container"] 
        [ viewJoinLeaveBtn session model 
        , viewOtherBtn session model
        ]
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

viewJoinLeaveBtn : Session -> Model -> Html Msg
viewJoinLeaveBtn session model =
  let
    joinLeaveText =
      if model.player == Nothing then "Join" else "Leave"
    joinLeaveMsg =
      if joinLeaveText == "Join" then JoinRoom session.player else LeaveRoom session.player
  in
  li [ class "control-item" ] 
     [ a [ onClick joinLeaveMsg ] [ text joinLeaveText ] ]

viewOtherBtn : Session -> Model -> Html Msg
viewOtherBtn session model =
  li [ class "control-item" ]
    [ a [ ] [ text "Some other button"]]

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    DoNothing -> ( (model, Cmd.none ), NoOp )
    NewMsg message -> ( ( model, Cmd.none), NoOp )
    Joined -> ( ( model, Cmd.none), NoOp)
    JoinedRoom -> 
      Debug.log ">>>>> RECEIVED JOINEDROOM MESSAGE <<<<<<<" ( ( model, Cmd.none ), NoOp )
    SocketOpened -> ( ( model, Cmd.none ), NoOp)
    SocketClosed -> ( (model, Cmd.none), NoOp )
    SocketClosedAbnormally -> ( ( model, Cmd.none), NoOp )
    Join (Just player) -> 
      ( ( { model | player = Just player, players = player :: model.players}, Cmd.none), NoOp )
    Join Nothing -> ( ( model, Cmd.none), NoOp)
    Leave (Just player) ->
      let
        filterBy =
          case model.player of
            Nothing -> ""
            Just player -> Player.usernameToString player.username
      in    
      ( ( { model | player = Nothing, players = 
            List.filter (\player -> Player.usernameToString(player.username) /= filterBy) model.players }
        , Cmd.none), NoOp )
    LeaveRoom Nothing -> ( ( model, Cmd.none), NoOp )

subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Phoenix.connect (socket session) model.channelSubscriptions