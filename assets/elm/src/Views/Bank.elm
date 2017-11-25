module Views.Bank exposing (view)

import Html as Html exposing (..)
import Html.Events as Events exposing (onClick)
import Html.Attributes as Attrs exposing (..)
import Json.Encode as Encode exposing (Value)
import Data.Player as Player exposing (Username)

view : model -> (String -> msg, String -> Value -> msg) -> Html msg
view model (setMsg, submitMsg) =
  div [ class "bank-container center-align" ]
    [ span [ class "modal-header red-text" ] [ text "Add chips?"] 
    , div [ class "bank-form-container" ] 
      [ div [ class "bank-form" ]
        [ div [ class "input-field" ]
          [ input 
            [ placeholder "How many chips would you like to add?"
            , type_ "number"
            , class "validate"
            ]
            []
          ]
        ]
      ]
    ]