module Page.Room.SocketConfig exposing (..)

import Data.AuthToken as AuthToken
import Data.Player as Player exposing (Player)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Data.Session as Session exposing (Session)
import Json.Encode as Encode
import Page.Room.Helpers as Helpers exposing (..)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Socket as Socket exposing (Socket)
import Types.Room.Messages as Messages exposing (..)


type alias Msg =
    RoomMsg


type alias Model =
    RoomPage


socket : Session -> String -> Socket Msg
socket session socketUrl =
    let
        params =
            case session.player of
                Just player ->
                    let
                        token =
                            AuthToken.authTokenToString player.token
                    in
                    [ ( "guardian_token", token ) ]

                Nothing ->
                    []
    in
    Socket.init socketUrl
        |> Socket.withParams params
        |> Socket.onOpen SocketOpened
        |> Socket.onClose (\_ -> SocketClosed)
        |> Socket.onAbnormalClose (\_ -> SocketClosedAbnormally)


room : Model -> Channel Msg
room model =
    Channel.init ("rooms:" ++ model.room)
        |> Channel.withPayload (Encode.object [ ( "type", Encode.string model.roomType ), ( "amount", Encode.int <| joinValToInt model.joinValue ) ])
        |> roomChannel


privateRoom : String -> Player -> Channel Msg
privateRoom roomTitle player =
    Channel.init ("rooms:" ++ roomTitle)
        |> Channel.withPayload (Encode.object [ ( "type", Encode.string "private" ), ( "amount", Encode.int 0 ) ])
        |> roomChannel


roomChannel : Channel Msg -> Channel Msg
roomChannel channel =
    channel
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
        |> Channel.on "new_message" (\payload -> NewMessage payload)


playerInfoChannel : Player -> Channel Msg
playerInfoChannel player =
    Channel.init ("players:" ++ Player.usernameToString player.username)
        |> Channel.onJoin (\_ -> ConnectedToPlayerChannel)
        |> Channel.on "chip_info" (\payload -> ChipInfo payload)



-- Channel helpers


subscribeToChannels : Player -> String -> String -> List (Channel Msg)
subscribeToChannels player roomTitle roomType =
    case roomType of
        "private" ->
            [ playerInfoChannel player, privateRoom roomTitle player ]

        _ ->
            [ playerInfoChannel player ]
