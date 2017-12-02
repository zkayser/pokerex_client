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

type Platform
  = Mobile
  | Desktop
  
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
        [ viewActionBtn actionsModel Call Desktop ]
      , div [ class "col s6" ]
        [ viewActionBtn actionsModel Raise Desktop ]
      ]
    , div [ class "row" ]
      [ div [ class "col s6"] 
        [ viewActionBtn actionsModel Check Desktop ]
      , div [ class "col s6" ]
        [ viewActionBtn actionsModel Fold Desktop ]
      , i [ class "material-icons close-modal", onClick actionsModel.closeModalMsg ]
        [ text "close" ]
      ]
    ]

viewMobile : ActionsModel msg -> Html msg
viewMobile actionsModel =
  div [ class "actions-container-mobile" ]
    [ viewActionBtn actionsModel Call Mobile
    , viewActionBtn actionsModel Raise Mobile
    , viewActionBtn actionsModel Check Mobile
    , viewActionBtn actionsModel Fold Mobile
    ]
    
encodeUsernamePayload : Username -> Value
encodeUsernamePayload username =
  Encode.object 
    [ ("player", Player.encodeUsername username) ]
    
actionBtnClass : String -> Platform -> String
actionBtnClass color platform =
  let
    extraClasses =
      case platform of
        Desktop -> ""
        Mobile -> "btn-floating"
  in   
  "waves-effect waves-effect-light btn white-text " ++ color ++ " " ++ extraClasses
  
viewActionBtn : ActionsModel msg -> Action -> Platform -> Html msg
viewActionBtn actionsModel action platform =
  let
    (color, message, btnText) =
      getAttributesFor actionsModel action
  in
  case canCallAction actionsModel action of
    True -> a [ class (actionBtnClass color platform), onClick message] [ childrenForBtn btnText platform ]
    False -> text ""

childrenForBtn : String -> Platform -> Html msg
childrenForBtn btnText platform =
  case platform of
    Desktop -> text btnText
    Mobile -> getIconForMobileBtn btnText

getIconForMobileBtn : String -> Html msg
getIconForMobileBtn btnText =
  case btnText of
    "Call" -> i [ class "material-icons" ] [ text "phone" ]
    "Raise" -> i [ class "material-icons" ] [ text "arrow_upward" ]
    "Check" -> i [  class "material-icons" ] [ text "check" ]
    "Fold" -> i [  class "material-icons" ] [ text "block" ]
    _ -> text ""
    
raiseContent : ActionsModel msg -> Html msg
raiseContent actionsModel =
  div [ class "raise-modal" ] 
    [
      div [ class "raise-modal-header" ] 
        [ h3 [ class "red-text" ] 
          [ text <| "Raise to " ++ (toString actionsModel.raiseAmount)  ]
        ]
    , input 
      [ type_ "number"
      , Attrs.min <| toString actionsModel.raiseMin
      , Attrs.max <| toString actionsModel.raiseMax
      , Attrs.placeholder "Enter your raise here"
      , onRangeChange actionsModel.setRaiseMsg
      ] []
    , div [ class "raise-buttons-container" ] 
      [ a [ onClick <| actionsModel.decreaseRaiseMsg actionsModel.raiseInterval, class "btn btn-large waves-effect raise-btn" ]
          [ i [ class "large material-icons" ] [ text "remove" ] ]
      , a [ class "btn btn-large green waves-effect white-text raise-submit", onClick <| raiseMsgWith actionsModel "action_raise" ] 
          [ text "Raise" ]
      , a [ onClick <| actionsModel.increaseRaiseMsg actionsModel.raiseInterval, class "btn btn-large waves-effect raise-btn" ]
          [ i [ class "large material-icons" ] [ text "add" ] ]
      ]
    , div [ class "raise-modal-close-row" ]
      [ i [ class "material-icons close-modal", onClick actionsModel.closeRaiseMsg ] [ text "close"] ]
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

getAttributesFor : ActionsModel msg -> Action -> (String, msg, String)
getAttributesFor actionsModel action =
  case action of
    Call -> ("blue darken-3", actionMsgWith actionsModel "action_call", "Call")
    Raise -> ("red darken-3", actionsModel.openRaiseMsg, "Raise")
    Check -> ("green accent-3", actionMsgWith actionsModel "action_check", "Check")
    Fold -> ("teal darken-4", actionMsgWith actionsModel "action_fold", "Fold")