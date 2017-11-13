module Views.Actions exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
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
  , closeRaiseMsg : msg
  , increaseRaiseMsg : (Int -> msg)
  , decreaseRaiseMsg : (Int -> msg)
  , setRaiseMsg : (String -> msg)
  , raiseAmount : Int
  , raiseMax : Int
  , raiseMin : Int
  , raiseInterval : Int
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
      , i [ class "material-icons close-modal", onClick actionsModel.closeModalMsg ]
        [ text "close" ]
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
    
raiseContent : ActionsModel msg -> Html msg
raiseContent actionsModel =
  div [ class "raise-modal" ] 
    [
      p [] [ text "Here is a raise panel."]
    , input 
      [ type_ "range"
      , Attrs.min <| toString actionsModel.raiseMin
      , Attrs.max <| toString actionsModel.raiseMax
      , onRangeChange actionsModel.setRaiseMsg
      ] []
    , i [ class "material-icons close-modal", onClick actionsModel.closeRaiseMsg ] [ text "close"]
    ]

actionMsgWith : ActionsModel msg -> String -> msg
actionMsgWith actionsModel pushMessage =
  actionsModel.actionMsg pushMessage (encodeUsernamePayload actionsModel.player)
  
raiseMsgWith : ActionsModel msg -> String -> msg
raiseMsgWith actionsModel pushMessage =
  actionsModel.actionMsg pushMessage 
    (Encode.object <| 
      [ ("player", Player.encodeUsername actionsModel.player)
      , ("amount", Encode.int actionsModel.raiseAmount)
      ])
  
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
      
onRangeChange : (String -> msg) -> Attribute msg
onRangeChange msg =
  on "change" <| Decode.map msg targetValue