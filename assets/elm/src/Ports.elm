port module Ports exposing (..)

import Json.Encode exposing (Value)

port storeSession : Maybe String -> Cmd msg

port logout : () -> Cmd msg

port triggerFBInviteRequest : () -> Cmd msg

port scrollChatToTop : () -> Cmd msg

port onSessionChange : (Value -> msg) -> Sub msg

port loginWithFB : () -> Cmd msg

port onFBLogin : (Value -> msg) -> Sub msg