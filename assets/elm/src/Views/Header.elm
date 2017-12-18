module Views.Header exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, Options)
import Html.Lazy exposing (lazy2)
import Util exposing (onClickStopPropagation)
import Route exposing (Route)
import Views.Helpers as Helpers exposing (ActivePage)
import Types.Page as Page exposing (Page)
import Types.Dropdowns as DropdownType exposing (OpenDropdown, DropdownMsg, DropdownItem, DropdownNavbarLink)
import Widgets.Dropdown as Dropdown

type alias NavDropdownInfo r =
  { r |
    openDropdown : OpenDropdown
  , selectedItem : DropdownItem
  }

viewNavBarLinks : msg -> Session -> ActivePage -> List (Html msg)
viewNavBarLinks msg session page =
  case session.player of
    Just player ->
      [ li [] [ a [ onClick msg ] [ text "Signout" ] ]
      , navBarLink (page == Helpers.Profile) (Route.Profile <| Player.usernameToString player.username)
        [ text "Profile" ]
      , navBarLink (page == Helpers.Room) (Route.Room "public" "room_1") [ text "Room" ]
      , navBarLink (page == Helpers.Rooms) Route.Rooms [ text "Rooms" ]
      ]
    Nothing ->
      [ navBarLink (page == Helpers.Login) Route.Login [ text "Login" ]
      , navBarLink (page == Helpers.Registration) Route.Register [ text "Signup" ]
      , navBarLink (page == Helpers.Room) (Route.Room "public" "room_1") [ text "Room" ]
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
    Page.Room _ -> Helpers.Room
    Page.Rooms _ -> Helpers.Rooms
    Page.Profile _ -> Helpers.Profile
    _ -> Helpers.Other

-- NavDropdown --
navDropdownConfig : Dropdown.Config DropdownMsg
navDropdownConfig =
  { topLevelHtml = i
    [  class "material-icons nav-dropdown-btn hide-on-large-only"
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

navLinks : Session -> List DropdownNavbarLink
navLinks session =
  case session.player of
    Just player -> [ DropdownType.Logout, DropdownType.Profile, DropdownType.Room, DropdownType.Rooms ]
    Nothing -> [ DropdownType.Login, DropdownType.Register ]
  --This id should come from index.Html

  --You can use it to scroll to the top of the page (by ID)
  --when switching pages in the pagination sense

bodyId : String
bodyId =
  "page-body"
