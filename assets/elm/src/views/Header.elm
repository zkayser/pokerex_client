module Views.Header exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, Options)
import Html.Lazy exposing (lazy2)
import Util exposing (onClickStopPropagation)
import Route exposing (Route)
import Views.Helpers as Helpers exposing (ActivePage(..))
import Types.Dropdowns as DropdownType exposing (OpenDropdown, DropdownMsg)
import Widgets.Dropdown as Dropdown

  --This id should come from index.Html

  --You can use it to scroll to the top of the page (by ID)
  --when switching pages in the pagination sense

bodyId : String
bodyId =
  "page-body"
