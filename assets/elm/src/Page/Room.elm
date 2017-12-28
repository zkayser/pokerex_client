module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Room as Room exposing (Room)
import Data.RoomPage as RoomPage exposing (RoomPage)
import Page.Room.SocketConfig as SocketConfig exposing (..)
import Page.Room.UpdateHelpers as Updaters exposing (..)
import Page.Room.Helpers as Helpers exposing (..)
import Page.Room.ViewHelpers exposing (..)
import Types.Room.ModalState as ModalState exposing (..)
import Types.Room.Messages as Messages exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)
import Time exposing (Time)
import Widgets.PlayerToolbar as PlayerToolbar
import Views.Actions as Actions
import Phoenix

-- Types
type alias Msg = RoomMsg
type alias ExternalMsg = RoomExternalMsg
type alias Model = RoomPage
type alias MessageType = RoomMessageType

-- INITIALIZATION --
initialModel : Player -> String -> String -> RoomPage
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

-- SUBSCRIPTIONS --
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions ]
    withBlur =
      case model.modalRendered of
        WinningHandModal _ -> Time.every 5000 ClearWinningHandModal
        _ -> Sub.none
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