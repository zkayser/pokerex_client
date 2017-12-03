module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Room as Room exposing (Room)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Data.Card as Card exposing (Card)
import Data.WinningHand as WinningHand exposing (WinningHand)
import Data.AuthToken as AuthToken
import Data.Chat
import Page.Room.SocketConfig as SocketConfig exposing (..)
import Types.Room.ModalState as ModalState exposing (..)
import Types.Room.Messages as Messages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)
import Dom.Scroll
import Dict exposing (Dict)
import Mouse
import Time exposing (Time)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Ports exposing (scrollChatToTop)
import Widgets.PlayerToolbar as PlayerToolbar
import Widgets.Modal as Modal
import Views.Actions as Actions
import Views.Bank as Bank
import Views.Account as Account
import Views.Chat as Chat exposing (Chat)
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Push as Push exposing (Push)

-- Boiler Plate

type alias Msg = RoomMsg 
  
type ExternalMsg = NoOp
      
type MessageType
  = RoomMessage String
  | ErrorMessage String

type alias Model = RoomPage

-- INITIALIZATION --
initialModel : Player -> String -> String -> Model
initialModel player roomTitle roomType =
  { room =  roomTitle
  , roomModel = Room.defaultRoom
  , roomType = roomType
  , roomMessages = []
  , players = []
  , player = player
  , joined = False
  , joinValue = "0"
  , channelSubscriptions = [ playerInfoChannel player ]
  , modalRendered = Closed
  , errorMessages = []
  , raiseAmount = 0
  , raiseInterval = 5
  , chipsAvailable = player.chips
  , chipsAvailableForJoin = player.chips
  , addAmount = 0
  , chat = []
  , currentChatMsg = ""
  }

-- VIEW --

view : Session -> Model -> Html Msg
view session model =
  let
    mobileToolbarView =
      case model.modalRendered of
        BottomModalOpen _ -> text ""
        _ -> PlayerToolbar.viewMobile (toolbarConfig model)
  in   
  div [ class "room-container" ] 
    [ div [ class "table-container" ]
      ((viewTableCenter model.roomModel) :: (viewTableCards model.roomModel) :: viewPlayers session model)
    , PlayerToolbar.view (toolbarConfig model)
    , mobileToolbarView
    , Actions.viewMobile (actionsViewConfig model)
    , maybeViewModal model
    , viewMessages model
    ]
  
viewPlayers : Session -> Model -> List (Html Msg)
viewPlayers session model =
  let
    (seating, chipRoll, playerHands, isActive) =
      (model.roomModel, model.roomModel.chipRoll, model.roomModel.playerHands, getIsActive model)
    seatingWithChipRoll =
      List.map (\seating -> 
        ( seating
        , model.player
        , Dict.get (Player.usernameToString seating.name) chipRoll
        , handWhereIs seating.name playerHands model.player
        , isActive))
        model.roomModel.seating
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
    [  span [ class "table-pot" ] 
      [ span [ class "table-pot-text" ] [ text "POT: " ]
      , text (toString room.pot) 
      ]
    ,  img [ id "deck", src "http://localhost:8081/images/card-back.svg.png"] []
    ]

viewTableCards : Room -> Html Msg
viewTableCards room =
  let
    cardsToHtml =
      case List.isEmpty room.table of
        True -> [ text "" ]
        False -> List.indexedMap (viewTableCard) room.table
  in
  div [ class "table-card-container" ] cardsToHtml
    
viewTableCard : Int -> Card -> Html Msg
viewTableCard index card =
  div [ class ("table-card table-card-" ++ (toString index)) ]
    [ Card.tableCardImageFor card ]

viewSeat : (Room.Seating, Player, Maybe Int, List Card, Bool) -> Html Msg
viewSeat (seating, player, maybeChipRoll, cards, isActive) =
  let 
    chipsToHtml =
      case maybeChipRoll of
        Nothing -> text ""
        Just chipCount -> text (toString chipCount)
    cardImages =
      List.indexedMap Card.playerHandCardImageFor cards
  in
  div [ 
        id ("seat-" ++ (toString (seating.position + 1)))
      , class "player-seat"
      , style [ ("text-align", "center") ] 
      , classList [("active-seat-" ++ (toString <| seating.position + 1), isActive && (seating.name == player.username))]
      ]
    ([ p [ class "player-emblem-name" ] [ Player.usernameToHtml seating.name ]
    , p [ class "player-chip-count" ] [ chipsToHtml ]
    ] ++ cardImages)

joinView : Model -> Html Msg
joinView model =
  div [ class "join-modal" ]
    [ h3 [ class "join-title red-text text-center" ] [ text "Join the Game" ]
    , h6 [ class "teal-text"] [ text <| "You currently have " ++ (toString model.chipsAvailableForJoin) ++ " chips." ]
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
        , class <| "btn white-text " ++ (if (joinValToInt model.joinValue) >= 100 then "green" else "gray")
        , disabled <| (joinValToInt model.joinValue) < 100 || (joinValToInt model.joinValue) > model.chipsAvailableForJoin
        , onClick Join 
        ] [ text "Join" ]
      ] 
    ]

maybeViewModal : Model -> Html Msg
maybeViewModal model =
  case model.modalRendered of
    JoinModalOpen -> Modal.view (joinModalConfig model)
    RaiseModalOpen -> Modal.view (raiseModalConfig model)
    BottomModalOpen Actions -> Modal.bottomModalView (actionsModalConfig model)
    BottomModalOpen Account -> Modal.bottomModalView (accountModalConfig model)
    BottomModalOpen ModalState.Chat -> Modal.bottomModalView (chatModalConfig model)
    BottomModalOpen MobileMenu -> Modal.bottomModalView (mobileMenuConfig model)
    BankModalOpen -> Modal.view (bankModalConfig model)
    WinningHandModal winningHand -> Modal.view (winningHandConfig winningHand model)
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
        
viewWinningHandContent : WinningHand -> Html Msg
viewWinningHandContent winningHand =
  div [ class "winning-hand-container" ]
    [ div [ class "winning-hand-message"]
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
  , bankPressedMsg = BankPressed
  , accountPressedMsg = AccountPressed
  , chatPressedMsg = ChatPressed
  , mobileToolbarPressed = MobileToolbarPressed
  , closeModalMsg = CloseModal
  }

joinModalConfig : Model -> Modal.Config Msg
joinModalConfig model =
  { classes = ["white"]
  , contentHtml = [ joinView model ]
  , styles = Nothing
  }
  
raiseModalConfig : Model -> Modal.Config Msg
raiseModalConfig model =
  { classes = ["white"]
  , contentHtml = [ Actions.raiseContent (actionsViewConfig model) ]
  , styles = Nothing
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
  { classes = ["white"]
  , contentHtml = [ Actions.view (actionsViewConfig model) ]
  , styles = Nothing  
  }

bankModalConfig : Model -> Modal.Config Msg
bankModalConfig model =
  { classes = ["white"]
  , contentHtml = [ Bank.view model (SetAddAmount, ActionMsg, CloseModal) ]
  , styles = Nothing
  }

accountModalConfig : Model -> Modal.Config Msg
accountModalConfig model =
  { classes = ["white"]
  , contentHtml = [ Account.view model.player ]
  , styles = Nothing
  }

chatModalConfig : Model -> Modal.Config Msg
chatModalConfig model =
  { classes = ["white"]
  , contentHtml = [ Chat.view model.chat model.currentChatMsg SetChatMsg SubmitChat CloseModal ]
  , styles = Just [ ("height", "40vh") ]
  }

mobileMenuConfig : Model -> Modal.Config Msg
mobileMenuConfig model =
  { classes = ["white"]
  , contentHtml = [ PlayerToolbar.viewMobileMenu <| toolbarConfig model ]
  , styles = Nothing
  }
  
winningHandConfig : WinningHand -> Model -> Modal.Config Msg
winningHandConfig winningHand model =
  { classes = ["white", "winning-hand-modal"]
  , contentHtml = [ viewWinningHandContent winningHand ] 
  , styles = Nothing 
  }

-- UPDATE --

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    JoinedChannel ->              handleJoinedChannel model
    Join ->                       handleJoin model
    JoinFailed value ->           handleJoinFailed model value
    SetJoinValue amount ->        handleSetJoinValue model amount
    Update payload ->             handleUpdate model payload
    GameStarted payload ->        handleUpdate model payload
    WinnerMessage payload ->      handleWinnerMessage model payload
    PresentWinningHand payload -> handlePresentWinningHand model payload
    SetBankInfo payload ->        handleSetBankInfo model payload
    Clear _ ->                    handleClear model
    ConnectedToPlayerChannel ->   ( ( model, Cmd.none), NoOp )
    ChipInfo payload ->           handleChipInfo model payload
    ActionPressed ->              ( ( { model | modalRendered = BottomModalOpen Actions }, Cmd.none), NoOp )
    ActionMsg action val ->       handleActionMsg model action val
    NewChatMsg value ->           handleNewChatMsg model value
    BankPressed ->                handleBankPressed model
    AccountPressed ->             handleAccountPressed model
    MobileToolbarPressed ->       ( ( { model | modalRendered = BottomModalOpen MobileMenu }, Cmd.none), NoOp)
    ChatPressed ->                ( ( { model | modalRendered = BottomModalOpen ModalState.Chat }, Cmd.none), NoOp )
    CloseRaiseModal ->            ( ( { model | modalRendered = BottomModalOpen Actions }, Cmd.none), NoOp )
    IncreaseRaise amount ->       handleIncreaseRaise model amount
    DecreaseRaise amount ->       handleDecreaseRaise model amount
    SetRaise amount ->            handleSetRaise model amount
    SetAddAmount amount ->        handleSetAddAmount model amount
    SetChatMsg message ->         handleSetChatMsg model message
    SubmitChat ->                 handleSubmitChat model
    SocketOpened ->               ( ( model, Cmd.none), NoOp )
    SocketClosed ->               ( ( model, Cmd.none), NoOp )
    SocketClosedAbnormally ->     ( ( model, Cmd.none), NoOp )
    Rejoined _ ->                 handleRejoin model
    JoinRoom player ->            handleJoinRoom model player
    Blur ->                       ( ( { model | modalRendered = Closed }, Cmd.none), NoOp)
    OpenRaisePressed ->           ( ( { model | modalRendered = RaiseModalOpen }, Cmd.none), NoOp)
    ClearErrorMessage _ ->        clearErrorMessage model
    ClearRoomMessage _ ->         clearRoomMessage model
    ClearWinningHandModal _ ->    clearWinningHandModal model
    CloseWinningHandModal ->      clearWinningHandModal model
    CloseModal ->                 ( ( { model | modalRendered = Closed }, Cmd.none), NoOp )
    LeaveRoom player ->           handleLeaveRoom player model

-- UPDATE HELPERS --

handleLeaveRoom : Player -> Model -> ( (Model, Cmd Msg), ExternalMsg )
handleLeaveRoom player model =
  let
    payload = 
      Actions.encodeUsernamePayload model.player.username
    actionMsg =
      "action_leave"
    phoenixPush =
      actionPush model.room actionMsg payload
  in
  ( ( {model | joined = False }, phoenixPush), NoOp )

handleJoin : Model -> ( (Model, Cmd Msg), ExternalMsg )
handleJoin model =
  let
    newSubscriptions =
      (room model) :: model.channelSubscriptions
  in
  ( ( { model | channelSubscriptions = newSubscriptions}, Cmd.none), NoOp )

handleJoinRoom : Model -> Player -> ( ( Model, Cmd Msg), ExternalMsg)
handleJoinRoom model player =
  let
    cmd =
      playerInfoPush (Player.usernameToString player.username) "get_chip_count"
  in
  ( ( { model | modalRendered = JoinModalOpen, joined = True }, cmd), NoOp)

handleSetJoinValue : Model -> String -> ( ( Model, Cmd Msg),  ExternalMsg)
handleSetJoinValue model stringAmount =
  ( ( { model | joinValue = toString <| joinValToInt stringAmount }, Cmd.none), NoOp)
      
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
    modalRendered =
      case model.modalRendered of
        WinningHandModal _ -> model.modalRendered
        _ -> Closed
    newModel =
      { model | roomModel = newRoom, modalRendered = modalRendered, raiseAmount = initRaiseAmount }
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
  
clearWinningHandModal : Model -> ( ( Model, Cmd Msg), ExternalMsg )
clearWinningHandModal model =
  ( ( { model | modalRendered = Closed }, Cmd.none), NoOp )
  
handleActionMsg : Model -> String -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleActionMsg model actionString value =
  let
      newModel =
        case actionString of
          "action_add_chips" -> { model | addAmount = 0, modalRendered = Closed }
          _ -> model
  in
  case List.member actionString possibleActions of
    False -> ( ( model, Cmd.none), NoOp )
    True -> ( ( newModel, actionPush model.room actionString value), NoOp)
    
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
        False -> 
          case amount < model.roomModel.toCall of
            True -> ( ( model, Cmd.none), NoOp )
            False -> ( ( { model | raiseAmount = abs amount }, Cmd.none), NoOp )
    Err _ -> ( ( model, Cmd.none), NoOp )

handleSetAddAmount : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleSetAddAmount model stringAmount =
    case String.toInt stringAmount of
      Ok amount -> 
        case amount >= 0 && amount <= model.chipsAvailable of
          True -> ( ( { model | addAmount = amount }, Cmd.none), NoOp )
          False -> ( ( model, Cmd.none), NoOp )
      Err _ -> ( ( { model | addAmount = 0 }, Cmd.none), NoOp )
    
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
    
handleWinnerMessage : Model -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleWinnerMessage model payload =
  case Decode.decodeValue (Decode.at ["message"] Decode.string) payload of
    Ok message -> 
      ( ( { model | roomMessages = message :: model.roomMessages }, Cmd.none), NoOp )
    _ -> ( ( model, Cmd.none), NoOp )
    
handlePresentWinningHand : Model -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handlePresentWinningHand model payload =
  case Decode.decodeValue WinningHand.decoder payload of
    Ok winningHand -> ( ( { model | modalRendered = WinningHandModal winningHand }, Cmd.none), NoOp )
    _ -> ( ( model, Cmd.none), NoOp )

handleSetBankInfo : Model -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleSetBankInfo model payload =
  case Decode.decodeValue (Decode.at ["chips"] Decode.int) payload of
    Ok chipsAvailable ->
      let
        player =
          model.player
        newPlayer =
          { player | chips = chipsAvailable }
      in
          
      ( ( { model | chipsAvailable = chipsAvailable, player = newPlayer }, Cmd.none), NoOp )
    _ -> ( ( model, Cmd.none), NoOp )

handleChipInfo : Model -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleChipInfo model payload =
  case Decode.decodeValue (Decode.at ["chips"] Decode.int) payload of
    Ok chipAmount -> ( ( { model | chipsAvailableForJoin = chipAmount }, Cmd.none), NoOp)
    Err _ -> ( ( model, Cmd.none), NoOp )

handleNewChatMsg : Model -> Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleNewChatMsg model payload =
  case Decode.decodeValue Data.Chat.decoder payload of
    Ok res ->
      let
        newChat =
          res :: model.chat
      in
      ( ( { model | chat = newChat }, scrollChatToTop () ), NoOp )
    _ -> ( ( model, Cmd.none ), NoOp )

handleBankPressed : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleBankPressed model =
  let
    payload =
      Encode.object [("player", Player.encodeUsername model.player.username) ]
    cmd =
      actionPush model.room "get_bank" payload 
   in
   ( ( { model | modalRendered = BankModalOpen }, cmd), NoOp)

handleAccountPressed : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleAccountPressed model =
  let
    payload =
      Encode.object [ ("player", Player.encodeUsername model.player.username) ]
    cmd =
      actionPush model.room "get_bank" payload
  in
  ( ( { model | modalRendered = BottomModalOpen Account }, cmd), NoOp )

handleClear : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleClear model =
  let
    seating =
      if model.joined then [{ name = model.player.username, position = 0 }] else []
    defaultRoom =
      Room.defaultRoom
    newRoom =
      { defaultRoom | seating = seating, chipRoll = model.roomModel.chipRoll }
  in
  ( ( { model | roomModel = newRoom }, Cmd.none), NoOp)

handleSetChatMsg : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetChatMsg model message =
  ( ( { model | currentChatMsg = message }, Cmd.none ), NoOp )

handleSubmitChat : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleSubmitChat model =
  case model.currentChatMsg of
    "" -> ( ( model, Cmd.none), NoOp )
    _ ->
      let
        payload =
          Data.Chat.encode model.player model.currentChatMsg
        push =
          Push.init ("rooms:" ++ model.room) "chat_msg"
            |> Push.withPayload payload
      in
      ( ( { model | currentChatMsg = "" }, Phoenix.push socketUrl push), NoOp )
      

-- PUSH MESSAGES --
actionPush : String -> String -> Value -> Cmd Msg
actionPush room actionString value =
  let
    push =
      Push.init ("rooms:" ++ room) actionString
        |> Push.withPayload value
  in
  Phoenix.push socketUrl push

playerInfoPush : String -> String -> Cmd Msg
playerInfoPush username msgToChannel =
  let
    push =
      Push.init ("players:" ++ username) msgToChannel
        |> Push.withPayload (Encode.object [("player", Encode.string username)])
  in
  Phoenix.push socketUrl push
  
-- SUBSCRIPTIONS --    

subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions ]
    withClearError =
      case model.errorMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearErrorMessage
    withClearRoomMessage =
      case model.roomMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearRoomMessage
  in
  Sub.batch (phoenixSubscriptions ++ [ withClearError, withClearRoomMessage ])
  
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

possibleActions : List String
possibleActions =
  [ "action_raise", "action_check", "action_call", "action_fold", "action_add_chips" ]

joinValToInt : String -> Int
joinValToInt stringAmount =
  case String.toInt stringAmount of
    Ok value -> value
    Err _ -> 0
