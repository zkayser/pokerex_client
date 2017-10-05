module Views.Page exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)

{-
Determines which navbar link will be rendered as active
-}

type ActivePage
  = Other
  | Login

frame : Bool -> Maybe Player -> ActivePage -> Html msg -> Html msg
frame isLoading player page content =
  div [ class "page-frame" ]
    [ viewHeader page player isLoading
    , content
    , viewFooter
    ]

viewHeader : ActivePage -> Maybe Player -> Bool -> Html msg
viewHeader page player isLoading =
  div [ style [ ("text-align", "center") ] ]
    [ text "HEADER" ]

viewFooter : Html msg
viewFooter =
  div [ style [ ("text-align", "center") ] ]
    [ text "FOOTER" ]

{-
  This id should come from index.Html

  You can use it to scroll to the top of the page (by ID)
  when switching pages in the pagination sense
-}
bodyId : String
bodyId =
  "page-body"
