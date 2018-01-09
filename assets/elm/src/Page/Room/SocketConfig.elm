module Page.Room.SocketConfig exposing (..)

import Data.Session as Session exposing (Session)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Data.Player as Player exposing (Player)
import Data.AuthToken as AuthToken
import Json.Encode as Encode
import Types.Room.Messages as Messages exposing (..)
import Page.Room.Helpers as Helpers exposing (..)
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)

type alias Msg = RoomMsg
type alias Model = RoomPage

socketUrl : String
socketUrl =
  "ws://localhost:8080/socket/websocket"

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
  Channel.init ("rooms:" ++ model.room)
    |> Channel.withPayload ( Encode.object [ ("type", Encode.string model.roomType), ("amount", Encode.int <| joinValToInt model.joinValue) ] )
    |> Channel.onJoin (\_ -> JoinedChannel)
    |> Channel.onJoinError (\json -> JoinFailed json)
    |> Channel.onRejoin (\json -> Rejoined json)
    |> Channel.on "update" (\payload -> Update payload)
    |> Channel.on "game_started" (\payload -> GameStarted payload)
    |> Channel.on "winner_message" (\payload -> WinnerMessage payload)
    |> Channel.on "present_winning_hand" (\payload -> PresentWinningHand payload)
    |> Channel.on "clear_ui" Clear
    |> Channel.on "bank_info" (\payload -> SetBankInfo payload)
    |> Channel.on "new_chat_msg" (\payload -> NewChatMsg payload)
    |> Channel.withDebug

playerInfoChannel : Player -> Channel Msg
playerInfoChannel player =
  Channel.init ("players:" ++ (Player.usernameToString player.username))
    |> Channel.onJoin (\_ -> ConnectedToPlayerChannel)
    |> Channel.on "chip_info" (\payload -> ChipInfo payload)