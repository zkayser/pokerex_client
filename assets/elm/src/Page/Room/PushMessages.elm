module Page.Room.PushMessages exposing (..)

import Page.Room.SocketConfig exposing (socketUrl)
import Types.Room.Messages as Messages exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix
import Phoenix.Push as Push

type alias Msg = RoomMsg

actionPush : String -> String -> Value -> Cmd Msg
actionPush room actionString value =
  let
    push =
      Push.init ("rooms:" ++ room) actionString
        |> Push.withPayload value
  in
  Phoenix.push socketUrl push

playerInfoPush : String -> String -> Cmd Msg
playerInfoPush username msgToChannel =
  let
    push =
      Push.init ("players:" ++ username) msgToChannel
        |> Push.withPayload (Encode.object [("player", Encode.string username)])
  in
  Phoenix.push socketUrl push