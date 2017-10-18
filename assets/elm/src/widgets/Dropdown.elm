module Widgets.Dropdown exposing (Context, Config, view)

import Html exposing (..)
import Html.Attributes exposing (style, class, classList)
import Html.Events exposing (onWithOptions)
import Types.Dropdowns as DropdownType exposing (DropdownItem) 
import Json.Decode as Decode

type alias Context =
  { selectedItem : DropdownItem 
  , isOpen : Bool
  }

type alias Config msg =
  { topLevelHtml : Html msg
  , clickedMsg : msg
  , itemPickedMsg : String -> msg
  }

view : Config msg -> Context -> List String -> Html msg
view config context data =
  let
    length = (List.length data) * 3
    displayStyles =
      if context.isOpen then
        [ ("transform", "scaleY(1)")
        , ("transform-origin", "top")
        , ("transition", "transform 1s ease-in-out") 
        , ("max-height", (toString length ) ++ "em")
        , ("transition", "max-height 0.5s ease-in-out")
        ]
      else
        [ ("transform", "scaleY(0)")
        , ("transform-origin", "top")
        , ("transition", "transform 1s ease-in-out")
        , ("max-height", "0")
        , ("transition", "max-height 0.5s ease-in-out")
        ]
  in
  ul 
    [ 
      style displayStyles 
      , classList [ ("dropdown-menu", context.isOpen), ("collection", True) ]
      , class "nav-dropdown"
    ]
    (List.map (viewItem config) data)

viewItem : Config msg -> String -> Html msg
viewItem config item =
  li [ onClick (config.itemPickedMsg item), class "collection-item nav-dropdown-item" ] 
    [ span [ class "cursor-pointer" ] [ text item ] ]   

-- Helper to cancel click anywhere --
onClick : msg -> Attribute msg
onClick message =
  onWithOptions
    "click"
    { stopPropagation = True
    , preventDefault = False
    }
    (Decode.succeed message)