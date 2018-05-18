module Page.Room.ViewHelpers exposing (..)

import Data.Card as Card exposing (Card)
import Data.Player as Player exposing (Player)
import Data.Room as Room exposing (Room)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Data.Session as Session exposing (Session)
import Data.WinningHand as WinningHand exposing (WinningHand)
import Dict exposing (Dict)
import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Page.Room.Helpers exposing (..)
import Types.Room.Messages as Messages exposing (RoomMessageType(..), RoomMsg(..))
import Types.Room.ModalState as ModalState exposing (..)
import Views.Account as Account
import Views.Actions as Actions
import Views.Bank as Bank
import Views.Chat as Chat exposing (Chat)
import Widgets.Modal as Modal
import Widgets.PlayerToolbar as PlayerToolbar


type alias Model =
    RoomPage


type alias Msg =
    RoomMsg


type alias MessageType =
    RoomMessageType


viewPlayers : Session -> Model -> List (Html Msg)
viewPlayers session model =
    let
        ( seating, chipRoll, playerHands, isActive ) =
            ( model.roomModel, model.roomModel.chipRoll, model.roomModel.playerHands, getIsActive model )

        seatingWithChipRoll =
            List.map
                (\seating ->
                    ( seating
                    , model.player
                    , Dict.get (Player.usernameToString seating.name) chipRoll
                    , handWhereIs seating.name playerHands model.player
                    , isActive
                    , model.roomModel
                    )
                )
                model.roomModel.seating
    in
    List.map viewSeat seatingWithChipRoll


viewTableCenter : Room -> Html Msg
viewTableCenter room =
    let
        tableCardsToView =
            case List.isEmpty room.table of
                True ->
                    [ text "" ]

                False ->
                    List.indexedMap viewTableCard room.table
    in
    div [ class "table-center" ]
        [ span [ class "table-pot" ]
            [ span [ class "table-pot-text" ] [ text "POT: " ]
            , text (toString room.pot)
            ]
        , img [ id "deck", src "https://poker-ex.herokuapp.com/images/card-back.svg.png" ] []
        ]


viewTableCards : Room -> Html Msg
viewTableCards room =
    let
        cardsToHtml =
            case List.isEmpty room.table of
                True ->
                    [ text "" ]

                False ->
                    List.indexedMap viewTableCard room.table
    in
    div [ class "table-card-container" ] cardsToHtml


viewTableCard : Int -> Card -> Html Msg
viewTableCard index card =
    div [ class ("table-card table-card-" ++ toString index) ]
        [ Card.tableCardImageFor card ]


viewSeat : ( Room.Seating, Player, Maybe Int, List Card, Bool, Room ) -> Html Msg
viewSeat ( seating, player, maybeChipRoll, cards, isActive, room ) =
    let
        chipsToHtml =
            case maybeChipRoll of
                Nothing ->
                    text ""

                Just chipCount ->
                    text (toString chipCount)

        cardImages =
            List.indexedMap Card.playerHandCardImageFor cards

        activePlayer =
            case room.active of
                Just active ->
                    active

                Nothing ->
                    Player.Username ""
        isPlayerLeaving =
            List.member seating.name room.leaving
    in
    div
        [ id ("seat-" ++ toString (seating.position + 1))
        , class "player-seat"
        , style [ ( "text-align", "center" ) ]
        , classList [ ( "active-seat-" ++ (toString <| seating.position + 1), Player.equals seating.name activePlayer )
                    , ( "seat-leaving", isPlayerLeaving )
                    ]
        ]
        ([ p [ class "player-emblem-name" ] [ Player.usernameToHtml seating.name ]
         , p [ class "player-chip-count" ] [ chipsToHtml ]
         ]
            ++ cardImages
        )


joinView : Model -> Html Msg
joinView model =
    div [ class "join-modal" ]
        [ h3 [ class "join-title red-text text-center" ] [ text "Join the Game" ]
        , h6 [ class "teal-text" ] [ text <| "You currently have " ++ toString model.chipsAvailableForJoin ++ " chips." ]
        , Html.form
            [ class "join-form"
            , onSubmit Join
            ]
            [ input
                [ type_ "number"
                , placeholder "Enter the number of chips you'd like to bring to the table"
                , onInput SetJoinValue
                ]
                []
            , button
                [ type_ "submit"
                , class <|
                    "btn white-text "
                        ++ (if joinValToInt model.joinValue >= 100 then
                                "green"
                            else
                                "gray"
                           )
                , disabled <| joinValToInt model.joinValue < 100 || joinValToInt model.joinValue > model.chipsAvailableForJoin
                , onClick Join
                ]
                [ text "Join" ]
            ]
        ]


maybeViewModal : Model -> Html Msg
maybeViewModal model =
    case model.modalRendered of
        JoinModalOpen ->
            Modal.view (joinModalConfig model)

        RaiseModalOpen ->
            Modal.view (raiseModalConfig model)

        BottomModalOpen Actions ->
            Modal.bottomModalView (actionsModalConfig model)

        BottomModalOpen Account ->
            Modal.bottomModalView (accountModalConfig model)

        BottomModalOpen ModalState.Chat ->
            Modal.bottomModalView (chatModalConfig model)

        BottomModalOpen MobileMenu ->
            Modal.bottomModalView (mobileMenuConfig model)

        BankModalOpen ->
            Modal.view (bankModalConfig model)

        WinningHandModal winningHand ->
            Modal.view (winningHandConfig winningHand model)

        Closed ->
            text ""


viewMessages : Model -> Html Msg
viewMessages model =
    let
        errorMessages =
            case model.errorMessages of
                [] ->
                    []

                _ ->
                    List.map (\msg -> ErrorMessage msg) model.errorMessages

        roomMessages =
            case model.roomMessages of
                [] ->
                    []

                _ ->
                    List.map (\msg -> RoomMessage msg) model.roomMessages

        messagesToView =
            errorMessages ++ roomMessages
    in
    case messagesToView of
        [] ->
            text ""

        _ ->
            div [ class "room-message-container" ] <|
                List.map viewMessage messagesToView


viewMessage : MessageType -> Html Msg
viewMessage messageType =
    case messageType of
        RoomMessage roomMessage ->
            div [ class "message room-message" ]
                [ text roomMessage ]

        ErrorMessage errorMessage ->
            div [ class "message error-message" ]
                [ text errorMessage ]


viewWinningHandContent : WinningHand -> Html Msg
viewWinningHandContent winningHand =
    div [ class "winning-hand-container" ]
        [ div [ class "winning-hand-message" ]
            [ h3 [ class "teal-text" ]
                [ text <| winningHand.winner ++ " wins with " ++ winningHand.handType ]
            ]
        , div [ class "winning-hand-cards" ]
            (List.map viewWinningCard winningHand.cards)
        , div [ class "winning-hand-close-row" ]
            [ i [ class "material-icons", onClick CloseWinningHandModal ] [ text "close" ] ]
        ]


viewWinningCard : Card -> Html Msg
viewWinningCard card =
    img [ src (Card.sourceUrlForCardImage card) ] []



-- WIDGET CONFIGS --


toolbarConfig : Model -> PlayerToolbar.Config Msg
toolbarConfig model =
    let
        hasJoined =
            List.member model.player.username (List.map .name model.roomModel.seating)

        ( txt, msg ) =
            if hasJoined then
                ( "Leave", LeaveRoom model.player )
            else
                ( "Join", JoinRoom model.player )

        hasLeft =
            List.member model.player.username model.roomModel.leaving

        isActive =
            getIsActive model
    in
    { joinLeaveMsg = msg
    , btnText = txt
    , actionPressedMsg = ActionPressed
    , isActive = isActive
    , bankPressedMsg = BankPressed
    , accountPressedMsg = AccountPressed
    , chatPressedMsg = ChatPressed
    , mobileToolbarPressed = MobileToolbarPressed
    , closeModalMsg = CloseModal
    , isLeaving = hasLeft
    }


joinModalConfig : Model -> Modal.Config Msg
joinModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ joinView model ]
    , styles = Nothing
    }


raiseModalConfig : Model -> Modal.Config Msg
raiseModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ Actions.raiseContent (actionsViewConfig model) ]
    , styles = Nothing
    }


actionsViewConfig : Model -> Actions.ActionsModel Msg
actionsViewConfig model =
    let
        isActive =
            getIsActive model

        chips =
            getChips model model.roomModel.chipRoll

        paidInRound =
            getChips model model.roomModel.round
    in
    { isActive = isActive
    , chips = chips
    , paidInRound = paidInRound
    , toCall = model.roomModel.toCall
    , player = model.player.username
    , actionMsg = ActionMsg
    , openRaiseMsg = OpenRaisePressed
    , closeModalMsg = Blur
    , closeRaiseMsg = CloseRaiseModal
    , increaseRaiseMsg = IncreaseRaise
    , decreaseRaiseMsg = DecreaseRaise
    , setRaiseMsg = SetRaise
    , raiseAmount = model.raiseAmount
    , raiseMax = chips
    , raiseMin = 0
    , raiseInterval = model.raiseInterval
    }


actionsModalConfig : Model -> Modal.Config Msg
actionsModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ Actions.view (actionsViewConfig model) ]
    , styles = Nothing
    }


bankModalConfig : Model -> Modal.Config Msg
bankModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ Bank.view model ( SetAddAmount, ActionMsg, CloseModal ) ]
    , styles = Nothing
    }


accountModalConfig : Model -> Modal.Config Msg
accountModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ Account.view model.player ]
    , styles = Nothing
    }


chatModalConfig : Model -> Modal.Config Msg
chatModalConfig model =
    { classes = [ "white" ]
    , contentHtml = [ Chat.view model.chat model.currentChatMsg SetChatMsg SubmitChat CloseModal ]
    , styles = Just [ ( "height", "40vh" ) ]
    }


mobileMenuConfig : Model -> Modal.Config Msg
mobileMenuConfig model =
    { classes = [ "white" ]
    , contentHtml = [ PlayerToolbar.viewMobileMenu <| toolbarConfig model ]
    , styles = Nothing
    }


winningHandConfig : WinningHand -> Model -> Modal.Config Msg
winningHandConfig winningHand model =
    { classes = [ "white", "winning-hand-modal" ]
    , contentHtml = [ viewWinningHandContent winningHand ]
    , styles = Nothing
    }
