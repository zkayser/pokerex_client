module Views.Chat exposing (Chat, view)

import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events

type alias PlayerName = String
type alias Message = String
type alias ChatMessage = (PlayerName, Message)

type alias Chat = 
  List ChatMessage

view : Chat -> Html msg
view chat =
  div [ class "chat-wrapper" ]
    [ span [ class "modal-header red-text" ] [ text "Chat" ]
    , ul [ class "chat-container collection" ]
      (List.map viewMessage chat)
    ]

viewMessage : ChatMessage -> Html msg
viewMessage (player, message) =
  li [ class "collection-item message-container" ] 
    [ span [ class "chat-player-name red-text" ]
      [ text <| player ++ " says: "]
    , span [ class "chat-message-item" ]
      [ text message ]
    ]