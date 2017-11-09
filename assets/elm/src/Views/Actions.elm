module Views.Actions exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Data.Player as Player exposing (Username)

type alias ActionsModel msg =
  { isActive : Bool
  , chips : Int
  , toCall : Int
  , player : Username
  , actionMsg : (String -> Value -> msg)
  , openRaiseMsg : msg
  , closeModalMsg : msg
  }

view : ActionsModel msg -> Html msg
view actionsModel =
  div [ class "actions-container" ]
    [ span [ class "modal-header red-text"] [ text "Actions" ]
    , div [ class "row" ]
      [ div [ class "col s6"] 
        [ a [ class "waves-effect waves-effect-light btn blue darken-3 white-text"
            , onClick (actionsModel.actionMsg "action_call" (encodeUsernamePayload actionsModel.player)) ] 
          [ text "Call"]
        ]
      , div [ class "col s6" ]
        [ a [ class "waves-effect waves-effect-light btn red darken-3 white-text"
            , onClick actionsModel.openRaiseMsg ] 
          [ text "Raise"] 
        ]
      ]
    , div [ class "row" ]
      [ div [ class "col s6"] 
        [ a [ class "waves-effect waves-effect-light btn green accent-3 white-text"
            , onClick (actionsModel.actionMsg "action_check" (encodeUsernamePayload actionsModel.player))] 
          [ text "Check"] 
        ]
      , div [ class "col s6" ]
        [ a [ class "waves-effect waves-effect-light btn teal darken-4 white-text"
            , onClick (actionsModel.actionMsg "action_fold" (encodeUsernamePayload actionsModel.player))] 
          [ text "Fold" ] 
        ]
      ]
    ]
    
encodeUsernamePayload : Username -> Value
encodeUsernamePayload username =
  Encode.object 
    [ ("player", Player.encodeUsername username) ]