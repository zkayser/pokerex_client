module Views.Bank exposing (view)

import Data.Player as Player exposing (Player, Username)
import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode exposing (Value)


type alias Model r =
    { r
        | addAmount : Int
        , player : Player
        , chipsAvailable : Int
    }


view : Model r -> ( String -> msg, String -> Value -> msg, msg ) -> Html msg
view model ( setMsg, submitMsg, closeMsg ) =
    div [ class "bank-container center-align" ]
        [ span [ class "modal-header red-text" ] [ viewTitle model ]
        , h4 [ class "red-text" ] [ text <| "You currently have " ++ toString model.chipsAvailable ++ " chips available." ]
        , div [ class "bank-form-container" ]
            [ div [ class "bank-form" ]
                [ Html.form [ onSubmit (submitMsg "action_add_chips" (encodePayload model)) ]
                    [ div [ class "input-field" ]
                        [ input
                            [ placeholder "How many chips would you like to add?"
                            , type_ "number"
                            , class "validate"
                            , onInput setMsg
                            ]
                            []
                        ]
                    ]
                , button
                    [ type_ "submit"
                    , onClick (submitMsg "action_add_chips" (encodePayload model))
                    , class "btn blue white-text"
                    ]
                    [ text "Add chips" ]
                ]
            ]
        , i [ class "close-modal material-icons small", onClick closeMsg ] [ text "close" ]
        ]


viewTitle : Model r -> Html msg
viewTitle model =
    if model.addAmount == 0 then
        text "Add chips?"
    else
        text <| "Add " ++ toString model.addAmount ++ " chips?"


encodePayload : Model r -> Value
encodePayload model =
    Encode.object
        [ ( "player", Player.encodeUsername model.player.username )
        , ( "amount", Encode.int model.addAmount )
        ]
