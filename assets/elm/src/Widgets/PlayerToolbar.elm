module Widgets.PlayerToolbar exposing (Config, view, viewMobile, viewMobileMenu)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Config msg =
    { joinLeaveMsg : msg
    , btnText : String
    , actionPressedMsg : msg
    , isActive : Bool
    , bankPressedMsg : msg
    , accountPressedMsg : msg
    , chatPressedMsg : msg
    , mobileToolbarPressed : msg
    , closeModalMsg : msg
    }


view : Config msg -> Html msg
view config =
    div [ class "controls-container" ]
        [ li [ class "control-item" ]
            [ a [ onClick config.joinLeaveMsg ] [ text config.btnText ] ]
        , li [ class "control-item" ]
            [ viewActionBtn config ]
        , li [ class "control-item" ]
            [ a [ onClick config.accountPressedMsg ] [ text "Account" ] ]
        , li [ class "control-item" ]
            [ a [ onClick config.chatPressedMsg ] [ text "Chat" ] ]
        , li [ class "control-item" ]
            [ a [ onClick config.bankPressedMsg ] [ text "Bank" ] ]
        ]


viewActionBtn : Config msg -> Html msg
viewActionBtn config =
    if config.isActive then
        a [ onClick config.actionPressedMsg, class "control-active" ] [ text "Actions" ]
    else
        a [] [ text "" ]


viewMobile : Config msg -> Html msg
viewMobile config =
    div
        [ class "mobile-controls-container btn-floating btn-large white-text red waves-effect"
        , onClick config.mobileToolbarPressed
        ]
        [ i [ class "material-icons" ] [ text "add" ] ]


viewMobileMenu : Config msg -> Html msg
viewMobileMenu config =
    let
        baseClasses =
            "collection-item mobile-menu-item green-text"
    in
    ul [ class "collection" ]
        [ li [ class baseClasses ]
            [ a [ onClick config.joinLeaveMsg ] [ text config.btnText ]
            ]
        , li [ class baseClasses ]
            [ a [ onClick config.accountPressedMsg ] [ text "Account" ]
            ]
        , li [ class baseClasses, onClick config.chatPressedMsg ]
            [ a [ onClick config.chatPressedMsg ] [ text "Chat" ]
            ]
        , li [ class baseClasses, onClick config.bankPressedMsg ]
            [ a [ onClick config.bankPressedMsg ] [ text "Bank" ]
            ]
        , i [ class "material-icons close-modal close-mobile-menu", onClick config.closeModalMsg ]
            [ text "close" ]
        ]
