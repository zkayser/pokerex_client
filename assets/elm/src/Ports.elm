port module Ports exposing (onSessionChange, storeSession, scrollChatToTop)

import Json.Encode exposing (Value)

port storeSession : Maybe String -> Cmd msg

port scrollChatToTop : () -> Cmd msg

port onSessionChange : (Value -> msg) -> Sub msg
