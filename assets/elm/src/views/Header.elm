module Views.Header exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, Options)
import Html.Lazy exposing (lazy2)
import Util exposing (onClickStopPropagation)
import Route exposing (Route)
import Views.Helpers as Helpers exposing (ActivePage)
import Types.Page as Page exposing (Page)
import Types.Dropdowns as DropdownType exposing (OpenDropdown, DropdownMsg, DropdownItem)
import Widgets.Dropdown as Dropdown

type alias NavDropdownInfo r =
  { r |
    openDropdown : OpenDropdown
  , selectedItem : DropdownItem
  }

viewNavBarLinks : ActivePage -> List (Html msg)
viewNavBarLinks page =
  [ navBarLink (page == Helpers.Login) Route.Login [ text "Login" ]
  , navBarLink (page == Helpers.Registration) Route.Register [ text "Signup" ] 
  ]

navBarLink : Bool -> Route -> List (Html msg) -> Html msg
navBarLink isActive route linkContent =
  li [ classList [ ("active", isActive) ] ]
    [ a [ Route.href route ] linkContent ]

activePageFrom : Page -> ActivePage
activePageFrom page =
  case page of
    Page.Login _ -> Helpers.Login
    Page.Register _ -> Helpers.Registration
    Page.Home _ -> Helpers.Home
    _ -> Helpers.Other

-- NavDropdown --
navDropdownConfig : Dropdown.Config DropdownMsg
navDropdownConfig =
  { topLevelHtml = i 
    [  class "material-icons nav-dropdown-btn right always-right hide-on-large-only"
    , onClick (DropdownType.Toggle DropdownType.NavBarDropdown)
    ] [ text "reorder" ]
  , clickedMsg = DropdownType.Toggle DropdownType.NavBarDropdown
  , itemPickedMsg = DropdownType.NavItemPicked
  }

navDropdownContext : NavDropdownInfo r -> Dropdown.Context
navDropdownContext model =
  { selectedItem = model.selectedItem
  , isOpen = model.openDropdown == DropdownType.NavBarDropdown
  }

navLinks : List String
navLinks =
  [ "Login", "Signup" ]
  --This id should come from index.Html

  --You can use it to scroll to the top of the page (by ID)
  --when switching pages in the pagination sense

bodyId : String
bodyId =
  "page-body"
