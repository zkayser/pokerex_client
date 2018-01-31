module Views.Footer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)


view : Html msg
view =
    footer [ class "page-footer blue-grey darken-3 white-text bottom" ]
        [ div [ class "container" ]
            [ div [ class "row footer-row" ]
                [ div [ class "col m6 l6 s12 center-align-small" ]
                    [ h5 [ class "white-text" ] [ text "POKEREX" ]
                    , p [ class "grey-text text-lighten-4" ]
                        [ text "PokerEx was designed and developed by Zack Kayser. "
                        , text "Connect with me on Twitter at @kayserzl or on "
                        , a [ href "https://www.linkedin.com/in/zack-kayser-93b96b88" ] [ text "LinkedIn. " ]
                        , text "Feel free to check out the open-source code on "
                        , a [ href "https://github.com/zkayser/pokerex_client" ] [ text "Github" ]
                        , text "! "
                        , text "You can also check out the server-side Elixir/Phoenix project "
                        , a [ href "https://github.com/zkayser/poker_ex" ] [ text "here" ]
                        , text "."
                        ]
                    ]
                , div [ class "col m4 l4 offset-l2 s12" ]
                    [ h5 [ class "white-text center-align-small" ] [ text "Links" ]
                    , ul [ class "text-center" ]
                        [ li [] [ a [ href "https://github.com/zkayser/pokerex_client" ] [ text "Github" ] ]
                        , li [] [ a [ href "https://github.com/zkayser/poker_ex" ] [ text "Server Side Elixir Project" ] ]
                        , li [] [ a [ href "http://elm-lang.org/" ] [ text "Elm-lang home page" ] ]
                        , li [] [ a [ href "http://www.phoenixframework.org/" ] [ text "Phoenix Framework" ] ]
                        , li [] [ a [ href "http://elixir-lang.org/" ] [ text "Elixir Lang Home Page" ] ]
                        ]
                    ]
                ]
            ]
        , div [ class "footer-copyright" ]
            [ div [ class "container" ] [ text "©Zack Kayser, 2017" ] ]
        ]
