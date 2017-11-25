module Views.Bank exposing (view)

import Html as Html exposing (..)
import Html.Events as Events exposing (onClick, onInput, onSubmit)
import Html.Attributes as Attrs exposing (..)
import Json.Encode as Encode exposing (Value)
import Data.Player as Player exposing (Player, Username)

type alias Model r =
  { r |
    addAmount : Int
  , player : Player
  }

view : Model r -> (String -> msg, String -> Value -> msg) -> Html msg
view model (setMsg, submitMsg) =
  div [ class "bank-container center-align" ]
    [ span [ class "modal-header red-text" ] [ text "Add chips?"] 
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
            [ text "Add chips"]
        ]
      ]
    ]

encodePayload : Model r -> Value
encodePayload model =
  Encode.object [
    ("player", Player.encodeUsername model.player.username)
  , ("amount", Encode.int model.addAmount) 
  ]