module Widgets.Toast exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)

type alias Toast r =
  { r | errors : List String, messages : List String }

type ToastMessage = Message String | Error String

viewMessages : Toast r -> Html msg
viewMessages toastData =
  let
    errorMessages =
      case toastData.errors of
        [] -> []
        _ -> List.map (\msg -> Error msg) toastData.errors
    messages =
      case toastData.messages of
        [] -> []
        _ -> List.map (\msg -> Message msg) toastData.messages
    allMessages =
      messages ++ errorMessages
  in
  case allMessages of
    [] -> text ""
    _ ->
      div [ class "messages-container" ]
        <| List.map viewMessage allMessages

viewMessage : ToastMessage -> Html msg
viewMessage toastMessage =
  case toastMessage of
    Message message ->
      div [ class "message success-message" ] [ text message ]
    Error message ->
      div [ class "message error-message" ] [ text message ]