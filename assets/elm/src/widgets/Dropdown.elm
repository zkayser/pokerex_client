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
    displayStyle =
      if context.isOpen then
        ("display", "block")
      else
        ("display", "none")

  in
  ul [ style [ displayStyle ], classList [ ("dropdown-menu", context.isOpen), ("collection", True) ] ]
    (List.map (viewItem config) data)

viewItem : Config msg -> String -> Html msg
viewItem config item =
  li [ onClick (config.itemPickedMsg item), class "collection-item" ]
    [ text item ]   

-- Helper to cancel click anywhere --
onClick : msg -> Attribute msg
onClick message =
  onWithOptions
    "click"
    { stopPropagation = True
    , preventDefault = False
    }
    (Decode.succeed message)