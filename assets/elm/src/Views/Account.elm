module Views.Account exposing (view)

import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events exposing (onClick)
import Route
import Data.Player as Player exposing (Player)

view : Player -> Html msg
view player =
  div [ class "account-container" ]
    [ span [ class "modal-header red-text" ] [ text "Your Account" ] 
    , div [ class "row account-info"] 
      [ span [ class "account-info-item" ] [ h5 [ ] [ text <| "Username: " ++ (Player.usernameToString player.username )]]
      , span [ class "account-info-item" ] [ h5 [ ] [ text <| "Chips: " ++ (toString player.chips) ]]
      ]
    , div [ class "row account-info" ]
      [ span [ class "account-info-item"] [ h5 [ ] [ text <| "Email: " ++ player.email ] ] 
      , span [ class "account-info-item"] [ a [ Route.href Route.Home ] [ text "Go to your account" ] ]
      ]
    ]