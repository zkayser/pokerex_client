module Page.NotFound exposing (view)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, main_, text)
import Html.Attributes exposing (class, id, src, tabindex)


view : Session -> Html msg
view session =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Not Found" ] ]
