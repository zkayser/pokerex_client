module Page.Room.UpdateHelpers exposing (..)

import Data.Chat exposing (Chat)
import Data.Player as Player exposing (Player)
import Data.Room as Room exposing (Room)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Data.WinningHand as WinningHand exposing (WinningHand)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page.Room.Helpers exposing (..)
import Page.Room.PushMessages exposing (actionPush, playerInfoPush)
import Page.Room.SocketConfig exposing (room)
import Phoenix
import Phoenix.Push as Push exposing (Push)
import Ports exposing (scrollChatToTop)
import Types.Room.Messages as Messages exposing (..)
import Types.Room.ModalState as ModalState exposing (..)
import Views.Actions as Actions


type alias Model =
    RoomPage


type alias Msg =
    RoomMsg


type alias ExternalMsg =
    RoomExternalMsg


handleLeaveRoom : Player -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleLeaveRoom player model =
    let
        payload =
            Actions.encodeUsernamePayload model.player.username

        actionMsg =
            "action_leave"

        phoenixPush =
            actionPush model.room actionMsg payload model.socketUrl
    in
    ( ( { model | joined = False }, phoenixPush ), NoOp )


handleJoin : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleJoin model =
    let
        newSubscriptions =
            room model :: model.channelSubscriptions
    in
    ( ( { model | channelSubscriptions = newSubscriptions, modalRendered = Closed }, Cmd.none ), NoOp )


handleJoinRoom : Model -> Player -> ( ( Model, Cmd Msg ), ExternalMsg )
handleJoinRoom model player =
    let
        cmd =
            playerInfoPush (Player.usernameToString player.username) "get_chip_count" model.socketUrl
    in
    ( ( { model | modalRendered = JoinModalOpen, joined = True }, cmd ), NoOp )


handleSetJoinValue : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetJoinValue model stringAmount =
    ( ( { model | joinValue = toString <| joinValToInt stringAmount }, Cmd.none ), NoOp )


handleJoinedChannel : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleJoinedChannel model =
    let
        newMessage =
            "Welcome to " ++ model.room

        newModel =
            { model | roomMessages = newMessage :: model.roomMessages }
    in
    ( ( newModel, Cmd.none ), NoOp )


handleJoinFailed : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleJoinFailed model json =
    let
        -- The Ok message received is because the player is joining a private
        -- room, but has not yet joined with an amount. To force the player to
        -- enter an amount to join the game, we deny their auto join that the
        -- client performs for "private" room users and make them join manually.
        -- Subsequent auto-joins should be successful.
        newModel =
            case Decode.decodeValue (Decode.field "message" Decode.string) json of
                Ok message ->
                    let
                        newSubscriptions =
                            List.filter (\channel -> channel.topic /= ("rooms:" ++ model.room)) model.channelSubscriptions
                    in
                    { model | roomMessages = message :: model.roomMessages, channelSubscriptions = newSubscriptions }

                Err _ ->
                    let
                        message =
                            "An error occurred when trying to join the room. Please try again."
                    in
                    { model | errorMessages = message :: model.errorMessages }
    in
    ( ( newModel, Cmd.none ), NoOp )


handleUpdate : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleUpdate model payload =
    let
        newRoom =
            case Decode.decodeValue Room.decoder payload of
                Ok room ->
                    room

                Err _ ->
                    model.roomModel

        chips =
            getChips model newRoom.chipRoll

        initRaiseAmount =
            case (newRoom.toCall + 5) > chips of
                True ->
                    newRoom.toCall + chips

                False ->
                    newRoom.toCall + 5

        modalRendered =
            case model.modalRendered of
                WinningHandModal _ ->
                    model.modalRendered

                _ ->
                    Closed

        newModel =
            { model | roomModel = newRoom, modalRendered = modalRendered, raiseAmount = initRaiseAmount }
    in
    ( ( newModel, Cmd.none ), NoOp )


clearErrorMessage : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
clearErrorMessage model =
    let
        firstErrorMessage =
            case List.head model.errorMessages of
                Just string ->
                    string

                Nothing ->
                    ""

        newErrorMessages =
            List.filter (\str -> str /= firstErrorMessage) model.errorMessages

        newModel =
            { model | errorMessages = newErrorMessages }
    in
    ( ( newModel, Cmd.none ), NoOp )


clearRoomMessage : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
clearRoomMessage model =
    let
        firstRoomMessage =
            case List.head model.roomMessages of
                Just string ->
                    string

                Nothing ->
                    ""

        newRoomMessages =
            List.filter (\str -> str /= firstRoomMessage) model.roomMessages

        newModel =
            { model | roomMessages = newRoomMessages }
    in
    ( ( newModel, Cmd.none ), NoOp )


clearWinningHandModal : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
clearWinningHandModal model =
    ( ( { model | modalRendered = Closed }, Cmd.none ), NoOp )


handleActionMsg : Model -> String -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleActionMsg model actionString value =
    let
        newModel =
            case actionString of
                "action_add_chips" ->
                    { model | addAmount = 0, modalRendered = Closed }

                _ ->
                    model
    in
    case List.member actionString possibleActions of
        False ->
            ( ( model, Cmd.none ), NoOp )

        True ->
            ( ( newModel, actionPush model.room actionString value model.socketUrl ), NoOp )


handleRejoin : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleRejoin model =
    ( ( model, Cmd.none ), NoOp )


handleSetRaise : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetRaise model stringAmount =
    case String.toInt stringAmount of
        Ok amount ->
            let
                chips =
                    getChips model model.roomModel.chipRoll

                paidInRound =
                    getChips model model.roomModel.round
            in
            case amount >= (paidInRound + chips) of
                True ->
                    ( ( { model | raiseAmount = chips }, Cmd.none ), NoOp )

                False ->
                    case amount < model.roomModel.toCall of
                        True ->
                            ( ( model, Cmd.none ), NoOp )

                        False ->
                            ( ( { model | raiseAmount = abs amount }, Cmd.none ), NoOp )

        Err _ ->
            ( ( model, Cmd.none ), NoOp )


handleSetAddAmount : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetAddAmount model stringAmount =
    case String.toInt stringAmount of
        Ok amount ->
            case amount >= 0 && amount <= model.chipsAvailable of
                True ->
                    ( ( { model | addAmount = amount }, Cmd.none ), NoOp )

                False ->
                    ( ( model, Cmd.none ), NoOp )

        Err _ ->
            ( ( { model | addAmount = 0 }, Cmd.none ), NoOp )


handleIncreaseRaise : Model -> Int -> ( ( Model, Cmd Msg ), ExternalMsg )
handleIncreaseRaise model amount =
    let
        chips =
            getChips model model.roomModel.chipRoll

        paidInRound =
            getChips model model.roomModel.round
    in
    case (model.raiseAmount + amount) >= (paidInRound + chips) of
        True ->
            ( ( { model | raiseAmount = chips }, Cmd.none ), NoOp )

        False ->
            ( ( { model | raiseAmount = model.raiseAmount + amount }, Cmd.none ), NoOp )


handleDecreaseRaise : Model -> Int -> ( ( Model, Cmd Msg ), ExternalMsg )
handleDecreaseRaise model amount =
    case (model.raiseAmount - amount) <= model.roomModel.toCall of
        True ->
            ( ( model, Cmd.none ), NoOp )

        False ->
            ( ( { model | raiseAmount = model.raiseAmount - amount }, Cmd.none ), NoOp )


handleWinnerMessage : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleWinnerMessage model payload =
    case Decode.decodeValue (Decode.at [ "message" ] Decode.string) payload of
        Ok message ->
            ( ( { model | roomMessages = message :: model.roomMessages }, Cmd.none ), NoOp )

        _ ->
            ( ( model, Cmd.none ), NoOp )


handlePresentWinningHand : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handlePresentWinningHand model payload =
    case Decode.decodeValue WinningHand.decoder payload of
        Ok winningHand ->
            ( ( { model | modalRendered = WinningHandModal winningHand }, Cmd.none ), NoOp )

        _ ->
            ( ( model, Cmd.none ), NoOp )


handleSetBankInfo : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetBankInfo model payload =
    case Decode.decodeValue (Decode.at [ "chips" ] Decode.int) payload of
        Ok chipsAvailable ->
            let
                player =
                    model.player

                newPlayer =
                    { player | chips = chipsAvailable }
            in
            ( ( { model | chipsAvailable = chipsAvailable, player = newPlayer }, Cmd.none ), NoOp )

        _ ->
            ( ( model, Cmd.none ), NoOp )


handleChipInfo : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleChipInfo model payload =
    case Decode.decodeValue (Decode.at [ "chips" ] Decode.int) payload of
        Ok chipAmount ->
            ( ( { model | chipsAvailableForJoin = chipAmount }, Cmd.none ), NoOp )

        Err _ ->
            ( ( model, Cmd.none ), NoOp )


handleNewChatMsg : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleNewChatMsg model payload =
    case Decode.decodeValue Data.Chat.decoder payload of
        Ok res ->
            let
                newChat =
                    res :: model.chat
            in
            ( ( { model | chat = newChat }, scrollChatToTop () ), NoOp )

        _ ->
            ( ( model, Cmd.none ), NoOp )


handleNewMessage : Model -> Value -> ( ( Model, Cmd Msg ), ExternalMsg )
handleNewMessage model payload =
    case Decode.decodeValue (Decode.at [ "message" ] Decode.string) payload of
        Ok message ->
            ( ( { model | roomMessages = message :: model.roomMessages }, Cmd.none ), NoOp )

        Err _ ->
            ( ( model, Cmd.none ), NoOp )


handleBankPressed : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleBankPressed model =
    let
        payload =
            Encode.object [ ( "player", Player.encodeUsername model.player.username ) ]

        cmd =
            actionPush model.room "get_bank" payload model.socketUrl
    in
    ( ( { model | modalRendered = BankModalOpen }, cmd ), NoOp )


handleAccountPressed : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleAccountPressed model =
    let
        payload =
            Encode.object [ ( "player", Player.encodeUsername model.player.username ) ]

        cmd =
            actionPush model.room "get_bank" payload model.socketUrl
    in
    ( ( { model | modalRendered = BottomModalOpen Account }, cmd ), NoOp )


handleClear : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleClear model =
    let
        seating =
            if model.joined then
                [ { name = model.player.username, position = 0 } ]
            else
                []

        defaultRoom =
            Room.defaultRoom

        newRoom =
            { defaultRoom | seating = seating, chipRoll = model.roomModel.chipRoll }
    in
    ( ( { model | roomModel = newRoom }, Cmd.none ), NoOp )


handleSetChatMsg : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetChatMsg model message =
    ( ( { model | currentChatMsg = message }, Cmd.none ), NoOp )


handleSubmitChat : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSubmitChat model =
    case model.currentChatMsg of
        "" ->
            ( ( model, Cmd.none ), NoOp )

        _ ->
            let
                payload =
                    Data.Chat.encode model.player model.currentChatMsg

                push =
                    Push.init ("rooms:" ++ model.room) "chat_msg"
                        |> Push.withPayload payload
            in
            ( ( { model | currentChatMsg = "" }, Phoenix.push model.socketUrl push ), NoOp )
