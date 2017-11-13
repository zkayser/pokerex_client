module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Room as Room exposing (Room)
import Data.Card as Card exposing (Card)
import Data.AuthToken as AuthToken
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Mouse
import Time exposing (Time)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Widgets.PlayerToolbar as PlayerToolbar
import Widgets.Modal as Modal
import Views.Actions as Actions
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Push as Push exposing (Push)

-- Boiler Plate

type Msg
  = NewMsg String
  | Join
  | JoinedChannel
  | JoinRoom Player
  | JoinFailed Value
  | ActionPressed
  | ActionMsg String Encode.Value
  | OpenRaisePressed
  | CloseRaiseModal
  | IncreaseRaise Int
  | DecreaseRaise Int
  | SetRaise String
  | Update Value
  | GameStarted Value
  | LeaveRoom Player
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally
  | Rejoined Value
  | Blur
  | ClearErrorMessage Time
  | ClearRoomMessage Time

type ExternalMsg
  = NoOp
  
type ModalState
  = Closed
  | JoinModalOpen
  | BottomModalOpen BottomModalType
  | RaiseModalOpen
  
type BottomModalType
  = Actions
  | Account
  | Chat
  | Bank
  
type MessageType
  = RoomMessage String
  | ErrorMessage String

type alias Model =
  { room : String
  , roomModel : Room
  , roomType : String
  , roomMessages : List String
  , players : List Player
  , player : Player
  , channelSubscriptions : List (Channel Msg)
  , modalRendered : ModalState
  , errorMessages : List String
  , raiseAmount : Int
  , raiseInterval : Int
  }

-- SOCKET & CHANNEL CONFIG --

socketUrl : String
socketUrl =
  "ws://phoenix-experiment-zkayser.c9users.io:8080/socket/websocket"

socket : Session -> Socket Msg
socket session =
  let
    params =
      case session.player of
        Just player ->
          let
            token = AuthToken.authTokenToString player.token
          in
          [ ( "guardian_token", token )]
        Nothing -> []
  in
  Socket.init socketUrl
    |> Socket.withParams params
    |> Socket.onOpen (SocketOpened)
    |> Socket.onClose (\_ -> SocketClosed)
    |> Socket.onAbnormalClose (\_ -> SocketClosedAbnormally)

room : Model -> Channel Msg
room model =
  Channel.init ("rooms:" ++ model.room)
    |> Channel.withPayload ( Encode.object [ ("type", Encode.string "public"), ("amount", Encode.int 200) ] )
    |> Channel.onJoin (\_ -> JoinedChannel)
    |> Channel.onJoinError (\json -> JoinFailed json)
    |> Channel.onRejoin (\json -> Rejoined json)
    |> Channel.on "update" (\payload -> Update payload)
    |> Channel.on "game_started" (\payload -> GameStarted payload)
    |> Channel.withDebug


initialModel : Player -> String -> String -> Model
initialModel player roomTitle roomType =
  { room =  roomTitle -- Should be updated to take dynamic values on load
  , roomModel = Room.defaultRoom
  , roomType = roomType
  , roomMessages = []
  , players = []
  , player = player
  , channelSubscriptions = [ ] -- should be initialized to players:#{room_number}
  , modalRendered = Closed
  , errorMessages = []
  , raiseAmount = 0
  , raiseInterval = 5
  }

-- VIEW --

view : Session -> Model -> Html Msg
view session model =
  div [ class "room-container" ] 
    [ div [ class "table-container" ]
      ((viewTableCenter model.roomModel) :: viewPlayers session model)
    , PlayerToolbar.view (toolbarConfig model)
    , maybeViewModal model
    , viewMessages model
    ]
  
viewPlayers : Session -> Model -> List (Html Msg)
viewPlayers session model =
  let
    (seating, chipRoll, playerHands) =
      (model.roomModel.seating, model.roomModel.chipRoll, model.roomModel.playerHands)
    seatingWithChipRoll =
      List.map (\seating -> 
        (seating, Dict.get (Player.usernameToString seating.name) chipRoll, handWhereIs seating.name playerHands model.player))
        seating
  in
  List.map (viewSeat) seatingWithChipRoll
  
viewTableCenter : Room -> Html Msg
viewTableCenter room =
  let
    tableCardsToView =
      case List.isEmpty room.table of
        True -> [ text "" ]
        False -> List.indexedMap (viewTableCard) room.table
  in
  div [ class "table-center" ]
    [  span [ class "table-pot" ] [ text ("POT: " ++ (toString room.pot)) ]
    ,  img [ id "deck", src "http://phoenix-experiment-zkayser.c9users.io:8081/images/card-back.svg.png"] []
    ,  div [ class "table-card-container" ] tableCardsToView
    ]
    
viewTableCard : Int -> Card -> Html Msg
viewTableCard index card =
  div [ class ("table-card table-card-" ++ (toString index)) ]
    [ Card.tableCardImageFor card ]

viewSeat : (Room.Seating, Maybe Int, List Card) -> Html Msg
viewSeat (seating, maybeChipRoll, cards) =
  let 
    chipsToHtml =
      case maybeChipRoll of
        Nothing -> text ""
        Just chipCount -> text (toString chipCount)
    cardImages =
      List.indexedMap Card.playerHandCardImageFor cards
  in
  div [ id ("seat-" ++ (toString (seating.position + 1))), class "player-seat", style [("text-align", "center")] ]
    ([ p [ class "player-emblem-name" ] [ Player.usernameToHtml seating.name ]
    , p [ class "player-chip-count" ] [ chipsToHtml ]
    ] ++ cardImages)

joinView : Model -> Html Msg
joinView model =
  div [ class "card-content" ]
    [ span [ class "card-title" ] [ text "Join the Game" ]
    , p [] [ text "Enter the amount of chips you would like to bring to the table."]
    , p [] [ text "You must enter with a minimum of 100 chips."]
    , p [] [ text ("Current Chip Amount: " ++ (toString model.player.chips )) ]
    ]

viewJoinActions : Model -> Html Msg
viewJoinActions model =
  div [ class "card-action" ]
    [ a [ class "btn green", onClick Join ] [ text "Join" ] ] -- Needs editing later on

maybeViewModal : Model -> Html Msg
maybeViewModal model =
  case model.modalRendered of
    JoinModalOpen -> Modal.view (joinModalConfig model)
    RaiseModalOpen -> Modal.view (raiseModalConfig model)
    BottomModalOpen Actions -> Modal.bottomModalView (actionsModalConfig model)
    BottomModalOpen Account -> text ""
    BottomModalOpen Chat -> text ""
    BottomModalOpen Bank -> text ""
    Closed -> text ""

viewMessages : Model -> Html Msg
viewMessages model =
  let 
    errorMessages =
      case model.errorMessages of
        [] -> []
        _ -> List.map (\msg -> (ErrorMessage msg)) model.errorMessages
    roomMessages =
      case model.roomMessages of
        [] -> []
        _ -> List.map (\msg -> (RoomMessage msg)) model.roomMessages
    messagesToView =
      errorMessages ++ roomMessages
  in
  case messagesToView of
    [] -> text ""
    _ -> div [ class "room-message-container" ]
          <| List.map viewMessage messagesToView
    
viewMessage : MessageType -> Html Msg
viewMessage messageType =
  case messageType of
    RoomMessage roomMessage ->
      div [ class "message room-message" ]
        [ text roomMessage]
    ErrorMessage errorMessage ->
      div [ class "message error-message" ]
        [ text errorMessage ]

-- WIDGET CONFIGURATIONS --

toolbarConfig : Model -> PlayerToolbar.Config Msg
toolbarConfig model =
  let
    hasJoined =
      List.member model.player.username (List.map .name model.roomModel.players)
    (txt, msg) =
      if hasJoined then ("Leave", LeaveRoom model.player) else ("Join", JoinRoom model.player)
    isActive = getIsActive model
  in
  { joinLeaveMsg = msg 
  , btnText = txt 
  , actionPressedMsg = ActionPressed
  , isActive = isActive
  }

joinModalConfig : Model -> Modal.Config Msg
joinModalConfig model =
  { backgroundColor = "white"
  , contentHtml = [ joinView model, viewJoinActions model ]
  }
  
raiseModalConfig : Model -> Modal.Config Msg
raiseModalConfig model =
  { backgroundColor = "white"
  , contentHtml = 
    [ Actions.raiseContent (actionsViewConfig model) ]
  }
  
actionsViewConfig : Model -> Actions.ActionsModel Msg
actionsViewConfig model =
  let
    isActive = getIsActive model
    chips = getChips model model.roomModel.chipRoll
    paidInRound = getChips model model.roomModel.round
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
  { backgroundColor = "white"
  , contentHtml = [ Actions.view (actionsViewConfig model)]  
  }

-- UPDATE --

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    NewMsg message ->         ( ( model, Cmd.none), NoOp )
    JoinedChannel ->          handleJoinedChannel model
    Join ->                   handleJoin model
    JoinFailed value ->       handleJoinFailed model value
    Update payload ->         handleUpdate model payload
    GameStarted payload ->    handleUpdate model payload
    ActionPressed ->          ( ( { model | modalRendered = BottomModalOpen Actions }, Cmd.none), NoOp)
    ActionMsg action val ->   handleActionMsg model action val
    CloseRaiseModal ->        ( ( { model | modalRendered = BottomModalOpen Actions }, Cmd.none), NoOp )
    IncreaseRaise amount ->   handleIncreaseRaise model amount
    DecreaseRaise amount ->   handleDecreaseRaise model amount
    SetRaise amount ->        handleSetRaise model amount
    SocketOpened ->           ( ( model, Cmd.none), NoOp )
    SocketClosed ->           ( ( model, Cmd.none), NoOp )
    SocketClosedAbnormally -> ( ( model, Cmd.none), NoOp )
    Rejoined _ ->             handleRejoin model
    JoinRoom player ->        ( ( { model | modalRendered = JoinModalOpen }, Cmd.none), NoOp)
    Blur ->                   ( ( { model | modalRendered = Closed }, Cmd.none), NoOp)
    OpenRaisePressed ->       ( ( { model | modalRendered = RaiseModalOpen }, Cmd.none), NoOp)
    ClearErrorMessage _ ->    clearErrorMessage model
    ClearRoomMessage _ ->     clearRoomMessage model
    LeaveRoom player ->       handleLeaveRoom player model

-- UPDATE HELPERS --

handleLeaveRoom : Player -> Model -> ( (Model, Cmd Msg), ExternalMsg )
handleLeaveRoom player model =
  let
    filterBy = Player.usernameToString player.username
    newModel =
      { model | players = List.filter (\player -> Player.usernameToString(player.username) /= filterBy) model.players
              , channelSubscriptions = []
      }
  in
  ( (newModel, Cmd.none), NoOp )

handleJoin : Model -> ( (Model, Cmd Msg), ExternalMsg )
handleJoin model =
  let
    newSubscriptions =
      (room model) :: model.channelSubscriptions
  in
  ( ( { model | channelSubscriptions = newSubscriptions}, Cmd.none), NoOp )

handleJoinedChannel : Model -> ( (Model, Cmd Msg), ExternalMsg )
handleJoinedChannel model =
  let
    newMessage =
      "Welcome to " ++ model.room
    newModel =
      { model | roomMessages = newMessage :: model.roomMessages }
  in
  ( (newModel, Cmd.none), NoOp )

handleJoinFailed : Model -> Value -> ( (Model, Cmd Msg), ExternalMsg )
handleJoinFailed model json =
  let
    message =
      case Decode.decodeValue (Decode.field "message" Decode.string) json of
        Ok theMessage -> theMessage
        Err _ -> "An error occurred when trying to join the room. Please try again."
    newModel =
      { model | errorMessages = message :: model.errorMessages }
  in
  ( (newModel, Cmd.none), NoOp )
  
handleUpdate : Model -> Value -> ( (Model, Cmd Msg), ExternalMsg )
handleUpdate model payload =
  let
    newRoom =
      case Decode.decodeValue Room.decoder payload of
        (Ok room) -> room
        (Err _) -> model.roomModel
    chips =
      getChips model newRoom.chipRoll
    initRaiseAmount =
      case (newRoom.toCall + 5) > chips of
        True -> newRoom.toCall + chips
        False -> newRoom.toCall + 5
    newModel =
      { model | roomModel = newRoom, modalRendered = Closed, raiseAmount = initRaiseAmount }
  in
  ( (newModel, Cmd.none), NoOp)
  
clearErrorMessage : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
clearErrorMessage model =
  let
    firstErrorMessage =
      case List.head model.errorMessages of
        Just string -> string
        Nothing -> ""
    newErrorMessages =
      List.filter (\str -> str /= firstErrorMessage) model.errorMessages
    newModel =
      { model | errorMessages = newErrorMessages }
  in
  ( ( newModel, Cmd.none), NoOp )

clearRoomMessage : Model -> ( ( Model, Cmd Msg), ExternalMsg )
clearRoomMessage model =
  let
    firstRoomMessage =
      case List.head model.roomMessages of
        Just string -> string
        Nothing -> ""
    newRoomMessages =
      List.filter (\str -> str /= firstRoomMessage) model.roomMessages
    newModel =
      { model | roomMessages = newRoomMessages }
  in
  ( (newModel, Cmd.none), NoOp )
  
handleActionMsg : Model -> String -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleActionMsg model actionString value =
  case List.member actionString ["action_raise", "action_call", "action_check", "action_fold"] of
    False -> ( ( model, Cmd.none), NoOp )
    True -> ( ( model, actionPush model.room actionString value), NoOp)
    
handleRejoin : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleRejoin model =
  let
    newModel =
      { model | roomMessages = model.roomMessages ++ [ "Your connection has been re-established."] }
  in
  ( (newModel, Cmd.none), NoOp)
  
handleSetRaise : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
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
        True -> ( ( { model | raiseAmount = chips }, Cmd.none), NoOp )
        False -> ( ( { model | raiseAmount = amount }, Cmd.none), NoOp )
    Err _ -> ( ( model, Cmd.none), NoOp )
    
handleIncreaseRaise : Model -> Int -> ( ( Model, Cmd Msg), ExternalMsg )
handleIncreaseRaise model amount =
  let
    chips =
      getChips model model.roomModel.chipRoll
    paidInRound =
      getChips model model.roomModel.round
  in
  case (( model.raiseAmount + amount ) >= (paidInRound + chips))  of
    True -> ( ( { model | raiseAmount = chips }, Cmd.none), NoOp )
    False -> ( ( { model | raiseAmount = model.raiseAmount + amount }, Cmd.none), NoOp )
    
handleDecreaseRaise : Model -> Int -> ( ( Model, Cmd Msg), ExternalMsg )
handleDecreaseRaise model amount =
  case (model.raiseAmount - amount) <= model.roomModel.toCall of 
    True -> ( ( model, Cmd.none ), NoOp )
    False -> ( ( { model | raiseAmount = model.raiseAmount - amount }, Cmd.none), NoOp )
  
-- PUSH MESSAGES --
actionPush : String -> String -> Value -> Cmd Msg
actionPush room actionString value =
  let
    push =
      Push.init ("rooms:" ++ room) actionString
        |> Push.withPayload value
  in
  Phoenix.push socketUrl push
  
-- SUBSCRIPTIONS --    

subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions ]
    withBlur =
      case model.modalRendered of
        Closed -> Sub.none
        RaiseModalOpen -> Sub.none
        _ -> Mouse.clicks (always Blur)
    withClearError =
      case model.errorMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearErrorMessage
    withClearRoomMessage =
      case model.roomMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearRoomMessage
  in
  Sub.batch (phoenixSubscriptions ++ [ withBlur, withClearError, withClearRoomMessage ])
  
-- INTERNAL HELPER FUNCTIONS
handWhereIs : Player.Username -> List Room.PlayerHand -> Player -> List Card
handWhereIs username playerHands player =
  let
    theHand =
      case List.filter (\playerHand -> Player.equals username playerHand.player) playerHands of
        [] -> Nothing
        [playerHand] -> Just playerHand
        _ -> Nothing
    handForPlayer =
      case theHand of
        Just hand -> 
          if Player.equals hand.player player.username then
            hand.hand
          else 
            [ {rank = Card.RankError, suit = Card.SuitError}, {rank = Card.RankError, suit = Card.SuitError} ]
        _ -> [ { rank = Card.RankError, suit = Card.SuitError}, {rank = Card.RankError, suit = Card.SuitError} ]
  in
  handForPlayer
  
getChips : Model -> Dict String Int -> Int
getChips model dict =
  case Dict.get (Player.usernameToString model.player.username) dict of
    Nothing -> 0
    Just chips -> chips
    
getIsActive : Model -> Bool
getIsActive model =
  case model.roomModel.active of
    Nothing -> False
    Just username -> Player.equals model.player.username username