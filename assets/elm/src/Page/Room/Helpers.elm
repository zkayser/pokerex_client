module Page.Room.Helpers exposing (..)

import Data.Card as Card exposing (Card)
import Data.Player as Player exposing (Player)
import Data.Room as Room exposing (..)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Dict exposing (Dict)


type alias Model =
    RoomPage


handWhereIs : Player.Username -> List Room.PlayerHand -> Player -> List Card
handWhereIs username playerHands player =
    let
        theHand =
            case List.filter (\playerHand -> Player.equals username playerHand.player) playerHands of
                [] ->
                    Nothing

                [ playerHand ] ->
                    Just playerHand

                _ ->
                    Nothing

        handForPlayer =
            case theHand of
                Just hand ->
                    if Player.equals hand.player player.username then
                        hand.hand
                    else
                        [ { rank = Card.RankError, suit = Card.SuitError }, { rank = Card.RankError, suit = Card.SuitError } ]

                _ ->
                    [ { rank = Card.RankError, suit = Card.SuitError }, { rank = Card.RankError, suit = Card.SuitError } ]
    in
    handForPlayer


getChips : Model -> Dict String Int -> Int
getChips model dict =
    case Dict.get (Player.usernameToString model.player.username) dict of
        Nothing ->
            0

        Just chips ->
            chips


getIsActive : Model -> Bool
getIsActive model =
    case model.roomModel.active of
        Nothing ->
            False

        Just username ->
            Player.equals model.player.username username


possibleActions : List String
possibleActions =
    [ "action_raise", "action_check", "action_call", "action_fold", "action_add_chips" ]


joinValToInt : String -> Int
joinValToInt stringAmount =
    case String.toInt stringAmount of
        Ok value ->
            value

        Err _ ->
            0


formatTitle : String -> String
formatTitle title =
    String.split "%20" title
        |> String.join "_"
        |> String.split " "
        |> String.join "_"


actionMessageFor : Model -> String -> String
actionMessageFor model action =
    case model.roomModel.active of
        Nothing ->
            ""

        Just player ->
            case action of
                "action_call" ->
                    Player.usernameToString player ++ " called " ++ (toString <| model.roomModel.toCall)

                "action_raise" ->
                    Player.usernameToString player ++ " raised " ++ (toString <| model.raiseAmount)

                "action_fold" ->
                    Player.usernameToString player ++ " folded"

                "action_check" ->
                    Player.usernameToString player ++ " checked"

                _ ->
                    ""
