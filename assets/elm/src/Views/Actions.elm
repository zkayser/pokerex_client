module Views.Actions exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Data.Player as Player exposing (Username)

type Action
  = Raise
  | Fold
  | Check
  | Call
  
type alias ActionsModel msg =
  { isActive : Bool
  , chips : Int
  , paidInRound : Int
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
        [ viewActionBtn actionsModel Call ]
      , div [ class "col s6" ]
        [ viewActionBtn actionsModel Raise ]
      ]
    , div [ class "row" ]
      [ div [ class "col s6"] 
        [ viewActionBtn actionsModel Check ]
      , div [ class "col s6" ]
        [ viewActionBtn actionsModel Fold ]
      ]
    ]
    
encodeUsernamePayload : Username -> Value
encodeUsernamePayload username =
  Encode.object 
    [ ("player", Player.encodeUsername username) ]
    
actionBtnClass : String -> String
actionBtnClass color =
  "waves-effect waves-effect-light btn white-text " ++ color
  
viewActionBtn : ActionsModel msg -> Action -> Html msg
viewActionBtn actionsModel action =
  let
    (color, message, btnText) =
      case action of
        Call -> ("blue darken-3", actionMsgWith actionsModel "action_call", "Call")
        Raise -> ("red darken-3", actionsModel.openRaiseMsg, "Raise")
        Check -> ("green accent-3", actionMsgWith actionsModel "action_check", "Check")
        Fold -> ("teal darken-4", actionMsgWith actionsModel "action_fold", "Fold")
  in
  case canCallAction actionsModel action of
    True -> a [ class (actionBtnClass color), onClick message] [ text btnText ]
    False -> text ""

actionMsgWith : ActionsModel msg -> String -> msg
actionMsgWith actionsModel pushMessage =
  actionsModel.actionMsg pushMessage (encodeUsernamePayload actionsModel.player)
  
canCallAction : ActionsModel msg -> Action -> Bool
canCallAction actionsModel action =
  if actionsModel.isActive == False then
    False
  else
    case action of
      Call -> actionsModel.paidInRound < actionsModel.toCall
      Check -> actionsModel.paidInRound == actionsModel.toCall
      Raise -> (actionsModel.chips > actionsModel.toCall)
      Fold -> actionsModel.paidInRound < actionsModel.toCall