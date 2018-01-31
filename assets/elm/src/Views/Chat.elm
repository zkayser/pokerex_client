module Views.Chat exposing (Chat, view)

import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events
import Json.Encode as Encode exposing (Value)


type alias PlayerName =
    String


type alias Message =
    String


type alias ChatMessage =
    { playerName : String
    , message : String
    }


type alias Chat =
    List ChatMessage


view : Chat -> String -> (String -> msg) -> msg -> msg -> Html msg
view chat currentMsg inputMsg submitMsg closeModalMsg =
    div [ class "chat-wrapper" ]
        [ span [ class "modal-header red-text" ] [ text "Chat" ]
        , i [ class "close-chat material-icons", Events.onClick closeModalMsg ] [ text "close" ]
        , ul [ class "chat-container collection", id "chat" ]
            (List.map viewMessage chat)
        , Html.form [ class "chat-input-container", Events.onSubmit submitMsg ]
            [ chatInput currentMsg inputMsg
            , a [ class "btn blue white-text chat-submit-btn", Events.onClick submitMsg ] [ text "Send" ]
            , a [ class "btn-floating green white-text chat-submit-mobile", Events.onClick submitMsg ]
                [ i [ class "material-icons" ]
                    [ text "send" ]
                ]
            ]
        ]


viewMessage : ChatMessage -> Html msg
viewMessage message =
    li [ class "collection-item message-container" ]
        [ span [ class "chat-player-name red-text" ]
            [ text <| message.playerName ++ " says: " ]
        , span [ class "chat-message-item" ]
            [ text message.message ]
        ]


chatInput : String -> (String -> msg) -> Html msg
chatInput currentMsg inputMsg =
    input
        [ type_ "text"
        , class "chat-input"
        , value currentMsg
        , placeholder "Say something"
        , Events.onInput inputMsg
        ]
        []
