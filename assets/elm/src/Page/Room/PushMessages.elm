module Page.Room.PushMessages exposing (..)

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix
import Phoenix.Push as Push
import Types.Room.Messages as Messages exposing (..)


type alias Msg =
    RoomMsg


actionPush : String -> String -> Value -> String -> Cmd Msg
actionPush room actionString value socketUrl =
    let
        push =
            Push.init ("rooms:" ++ room) actionString
                |> Push.withPayload value
    in
    Phoenix.push socketUrl push


playerInfoPush : String -> String -> String -> Cmd Msg
playerInfoPush username msgToChannel socketUrl =
    let
        push =
            Push.init ("players:" ++ username) msgToChannel
                |> Push.withPayload (Encode.object [ ( "player", Encode.string username ) ])
    in
    Phoenix.push socketUrl push
