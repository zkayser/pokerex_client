module Views.Chat exposing (Chat, view)

import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events

type alias PlayerName = String
type alias Message = String
type alias ChatMessage = 
  { playerName : String
  , message : String
  }

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
viewMessage message =
  li [ class "collection-item message-container" ] 
    [ span [ class "chat-player-name red-text" ]
      [ text <| message.playerName ++ " says: "]
    , span [ class "chat-message-item" ]
      [ text message.message ]
    ]