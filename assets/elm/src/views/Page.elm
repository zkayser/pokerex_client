module Views.Page exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, Options)
import Html.Lazy exposing (lazy2)
import Util exposing (onClickStopPropagation)
import Route exposing (Route)
import Views.Helpers as Helpers exposing (ActivePage(..))
import Widgets.Dropdown as Dropdown


frame : Bool -> Maybe Player -> ActivePage -> Html msg -> Html msg
frame isLoading player page content =
  div [ class "page-frame" ]
    [ (viewHeader page player isLoading)
    , content ]

viewHeader : ActivePage -> Maybe Player -> Bool -> Html msg
viewHeader page player isLoading =
  nav [ class "nav-top teal darken-4"]
    [ div [ class "nav-wrapper valign-wrapper" ]
      [ a [ Route.href Route.Home, class "brand-logo center" ] [ text "PokerEx" ]
      , ul [ id "nav-mobile", class "right always-right hide-on-med-and-down" ]
        (viewNavBarLinks  page)
      ]
    ]

viewNavBarLinks : ActivePage -> List (Html msg)
viewNavBarLinks page =
  [ navBarLink (page == Login) Route.Login [ text "Login" ]
  , navBarLink (page == Registration) Route.Register [ text "Register" ] 
  ]

navBarLink : Bool -> Route -> List (Html msg) -> Html msg
navBarLink isActive route linkContent =
  li [ classList [ ("active", isActive) ] ]
    [ a [ Route.href route ] linkContent ]


  --This id should come from index.Html

  --You can use it to scroll to the top of the page (by ID)
  --when switching pages in the pagination sense

bodyId : String
bodyId =
  "page-body"
