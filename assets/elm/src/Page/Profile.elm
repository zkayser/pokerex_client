module Page.Profile exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile)
import Data.AuthToken as AuthToken
import Ports exposing (triggerFBInviteRequest)
import Widgets.Pagination as Pagination exposing (paginate)
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (class, placeholder, classList, style, href)
import Html.Events exposing (onClick, onSubmit, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Time exposing (Time)
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Push as Push exposing (Push)
import Phoenix

type alias Model =
  { player : Player
  , profile : Profile
  , activeAttribute : UpdatableAttribute
  , channelSubscriptions : List (Channel Msg)
  , updateMessages : List String
  , errorMessages : List String
  , currentTab : Tab
  , currentGames : Rooms
  , invitedGames : Rooms
  }

type alias Rooms = { rooms : RoomInfoList, page : Int, totalPages : Int}

type alias RoomInfo = { room : String, playerCount : Int, status : RoomStatus }

type alias RoomInfoList = List RoomInfo

type RoomStatus
  = Full
  | Empty
  | WaitingForPlayers
  | Active

type RoomListing
  = Invite
  | Ongoing

type Msg
  = UpdateEmail String
  | UpdateBlurb String
  | UpdateChips
  | SubmitEmailUpdate
  | SubmitBlurbUpdate
  | UpdatePlayer Decode.Value
  | UpdateCurrentRooms Decode.Value
  | AcceptInvitation String
  | HeaderClicked UpdatableAttribute
  | Paginate String String
  | NewUpdateMessage Decode.Value
  | NewErrorMessage Decode.Value
  | TabClicked Tab
  | FBInviteBtnClicked
  | ClearUpdateMessage Time
  | ClearErrorMessage Time
  | ConnectedToPlayerChannel
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

type MessageType
  = UpdateMessage String
  | ErrorMessage String

type UpdatableAttribute
  = Email
  | Chips
  | Blurb
  | None

type Tab
  = CurrentGames
  | StartPrivateGame

-- INITIALIZATION
initialModel : Player -> Model
initialModel player =
  { player = player
  , profile = profileFor player
  , activeAttribute = None
  , channelSubscriptions = [ playerChannel player, privateRoomsChannel player ]
  , updateMessages = []
  , errorMessages = []
  , currentTab = CurrentGames
  , currentGames = { rooms = [], page = 1, totalPages = 0}
  , invitedGames = { rooms = [], page = 1, totalPages = 0}
  }

profileFor : Player -> Profile
profileFor player =
  { errors = []
  , username = Player.usernameToString player.username
  , blurb = ""
  , id = 0
  , email = player.email
  , chips = player.chips
  , isNewProfile = False
  }

-- SOCKET CONFIG
socketUrl : String
socketUrl =
  "ws://localhost:8080/socket/websocket"

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

playerChannel : Player -> Channel Msg
playerChannel player =
  Channel.init ("players:" ++ (Player.usernameToString player.username))
    |> Channel.onJoin (\_ -> ConnectedToPlayerChannel)
    |> Channel.on "player" (\payload -> UpdatePlayer payload)
    |> Channel.on "attr_updated" (\message -> NewUpdateMessage message)
    |> Channel.on "error" (\error -> NewErrorMessage error)

privateRoomsChannel : Player -> Channel Msg
privateRoomsChannel player =
  Channel.init ("private_rooms:" ++ (Player.usernameToString player.username))
    |> Channel.on "current_rooms" (\payload -> UpdateCurrentRooms payload)
    |> Channel.on "error" (\error -> NewErrorMessage error)

-- PUSH MESSAGES
updatePlayerPush : Model -> UpdatableAttribute -> String -> Cmd Msg
updatePlayerPush model attribute value =
  let
    stringAttr =
      case attribute of
        Email -> "email"
        Blurb -> "blurb"
        Chips -> "chips"
        _ -> "error"
    push =
      Push.init ("players:" ++ (Player.usernameToString model.player.username)) "update_player"
        |> Push.withPayload (Encode.object [ (stringAttr, Encode.string value)])
  in
  Phoenix.push socketUrl push

getPlayerPush : Model -> Cmd Msg
getPlayerPush model =
  let
    push =
      Push.init ("players:" ++ (Player.usernameToString model.player.username)) "get_player"
        |> Push.withPayload (Encode.object [])
  in
  Phoenix.push socketUrl push

acceptInvitationPush : Model -> String -> Cmd Msg
acceptInvitationPush model room =
  let
    playerName =
      Player.usernameToString model.player.username
    push =
      Push.init ("private_rooms:" ++ playerName) "accept_invitation"
        |> Push.withPayload (Encode.object
            [ ("player", Encode.string playerName )
            , ("room", Encode.string room)
            ] )
  in
  Phoenix.push socketUrl push

-- VIEW
view : Session -> Model -> Html Msg
view session model =
  div [ class "profile-container"]
    [ (viewUpdateMessages model)
    , h1 [ class "teal-text profile-greeting" ]
      [ text <| (playerGreeting model.player) ]
    , div [ class "profile-pane-container"]
      [ div [ class "profile-pane" ]
        [ ul [ class "collapsible popout"]
          (viewProfileForm model)
        , div [ class "fb-invite-container" ]
          [ button [ onClick FBInviteBtnClicked, class "btn blue white-text" ]
            [ text "Invite your Facebook friends to PokerEx"]
          ]
        ]
      , div [ class "profile-pane" ]
        (viewTabs model)
      ]
    ]

viewProfileForm : Model -> List (Html Msg)
viewProfileForm model =
  [ (viewUsernameEditField model)
  , li [ classList [ ("active", model.activeAttribute == Email) ] ]
    [ viewEditHeaderFor Email model
    , viewEditFieldFor Email (UpdateEmail model.profile.email) model
    ]
  , li [ classList [ ("active", model.activeAttribute == Blurb ) ] ]
    [ viewEditHeaderFor Blurb model
    , viewEditFieldFor Blurb (UpdateBlurb model.profile.blurb) model
    ]
  , li [ classList [ ("active", model.activeAttribute == Chips)]]
    [ viewEditHeaderFor Chips model
    , viewEditFieldFor Chips UpdateChips model
    ]
  ]

viewUsernameEditField : Model -> Html Msg
viewUsernameEditField model =
  li [ ]
    [ div [ class "collapsible-header" ]
      [ text "Username: "
      , text model.profile.username
      ]
    ]

viewEditHeaderFor : UpdatableAttribute -> Model -> Html Msg
viewEditHeaderFor attribute model =
  let
    headerText =
      case attribute of
        Email -> "Email: " ++ model.profile.email
        Blurb -> "Blurb: " ++ model.profile.blurb
        Chips -> "Chips: " ++ (toString model.profile.chips)
        _ -> ""
  in
  div
    [ classList [ ("collapsible-header", True), ("active", model.activeAttribute == attribute )]
    , onClick (HeaderClicked attribute)
    ]
    [ (addEditIcon attribute model), text headerText ]

viewEditFieldFor : UpdatableAttribute -> Msg -> Model -> Html Msg
viewEditFieldFor attribute msg model =
  case attribute of
    Email ->
      div [ class "collapsible-body", styleBodyFor model attribute ]
        [ form [ onSubmit SubmitEmailUpdate ]
          [ div [ class "input-field" ]
            [ input [ placeholder model.profile.email, onInput UpdateEmail ] [] ]
          ]
        ]
    Blurb ->
      div [ class "collapsible-body", styleBodyFor model attribute ]
        [ form [ onSubmit SubmitBlurbUpdate ]
          [ div [ class "input-field" ]
            [ input [ placeholder model.profile.blurb, onInput UpdateBlurb ] [] ]
          ]
        ]
    Chips ->
      div [ class "collapsible-body chip-restore", styleBodyFor model attribute ]
      [ form [ onSubmit msg ]
        [ button [ class "btn blue white-text", onClick msg ]
          [ text "Restore chip count to 1000" ]
        ]
      ]
    _ -> text ""

addEditIcon : UpdatableAttribute -> Model -> Html Msg
addEditIcon attribute model =
  let
    editHtml =
      i [ class "material-icons medium teal-text" ] [ text "edit" ]
  in
  case attribute of
    Email -> editHtml
    Blurb -> editHtml
    Chips -> if model.player.chips <= 100 then editHtml else (text "")
    _ -> text ""

viewUpdateMessages : Model -> Html Msg
viewUpdateMessages model =
  let
    errorMessages =
      case model.errorMessages of
        [] -> []
        _ -> List.map (\msg -> (ErrorMessage msg)) model.errorMessages
    updateMessages =
      case model.updateMessages of
        [] -> []
        _ -> List.map (\msg -> (UpdateMessage msg)) model.updateMessages
    messagesToView =
      errorMessages ++ updateMessages
  in
  case messagesToView of
    [] -> text ""
    _ -> div [ class "room-message-container" ]
          <| List.map viewMessage messagesToView

-- TODO: Change `room-message` class to be `success-message`
-- and move viewMessages/viewMessage functions into a common module that
-- can be shared across modules. This same functionality is being used in
-- the Room module (in `ViewHelpers.elm`).
viewMessage : MessageType -> Html Msg
viewMessage message =
  case message of
    UpdateMessage updateMessage ->
      div [ class "message room-message" ] [ text updateMessage ]
    ErrorMessage errorMessage ->
      div [ class "message error-message" ] [ text errorMessage ]

viewTabs : Model -> List (Html Msg)
viewTabs model =
  [ div [ class "row tab-holder-row" ]
    [ div [ class "col s12 tab-holder" ]
      [ ul [ class "tabs" ]
        [ li [ class "tab col s6" ]
          [ a
            [ classList
              [ ("active", model.currentTab == CurrentGames)
              , ("active-tab", model.currentTab == CurrentGames)
              ]
            , onClick (TabClicked CurrentGames)
            ]
              [text "Current Games" ]
          ]
        , li [ class "tab col s6" ]
          [ a
            [ classList
              [ ("active", model.currentTab == StartPrivateGame)
              , ("active-tab", model.currentTab == StartPrivateGame)
              ]
            , onClick (TabClicked StartPrivateGame)
            ]
              [text "Start Private Game" ]
          ]
        ]
      ]
    ]
  , (viewCurrentTab model model.currentTab)
  ]

viewCurrentTab : Model -> Tab -> Html Msg
viewCurrentTab model tab =
  case tab of
    CurrentGames -> viewCurrentGameTab model
    StartPrivateGame -> viewStartPrivateGameTab model

viewCurrentGameTab : Model -> Html Msg
viewCurrentGameTab model =
  div [ class "row"]
    [ div [ class "current-games-tab-container col s12" ]
      [ div [ class "current-games" ]
        (viewCurrentGames model)
      , hr [] []
      , div [ class "invited-games" ]
        (viewInvitedGames model)
      ]
    ]

viewCurrentGames : Model -> List (Html Msg)
viewCurrentGames model =
  case model.currentGames.rooms of
    [] -> [ h5 [ class "game-info-header red-text"] [ text "Your Current Games"]
          , p [ class "game-info-text" ] [ text "You have no ongoing games"]
          , div [ class "row"]
            [ a
              [ class "game-info-text btn blue white-text waves-effect col s6 offset-s3"
              , onClick (TabClicked StartPrivateGame)
              ]
              [ text "Start a Game"]
            ]
          ]
    _ -> [ h5 [ class "game-info-header red-text"] [ text "Your Current Games"] ] ++
         [ paginate model.currentGames (paginationConfig Ongoing) ]++
          (List.map (viewRoom Ongoing) model.currentGames.rooms)

viewInvitedGames : Model -> List (Html Msg)
viewInvitedGames model =
  case model.invitedGames.rooms of
    [] -> [ h5 [ class "game-info-header purple-text" ] [ text "Your Invites"]
          , p [ class "game-info-text"] [ text "You currently have no invites"]
          ]
    _ ->
      [ h5 [ class "game-info-header purple-text"] [ text "Your Invites"] ] ++
      [ paginate model.invitedGames (paginationConfig Invite) ] ++
      (List.map (viewRoom Invite) model.invitedGames.rooms)


viewRoom : RoomListing -> RoomInfo -> Html Msg
viewRoom listingType roomInfo =
  li [ class <| "collection-item " ++ (toString roomInfo.status) ++ " room-info-item" ]
    [ div [ class "room-list-title" ]
      [ span [ class "teal-text" ]
        -- TODO: The anchor tag below should route to a `private room` route
        -- if `listingType` == `Ongoing`
        [ a [ ] [ text roomInfo.room ] ]
      ]
    , div [ class "room-list-status" ]
      ([ p
        [ class "room-list-player-count"]
        [ text <| "Active Players: " ++ (toString roomInfo.playerCount) ]
      , p
        [ class <| "status " ++ (toString roomInfo.status)]
        [text <| "Status: " ++ (statusToString roomInfo.status)]

      ] ++ (maybeViewJoinDeclineBtns listingType roomInfo))
    , hr [] []
    ]

maybeViewJoinDeclineBtns : RoomListing -> RoomInfo -> List (Html Msg)
maybeViewJoinDeclineBtns listingType roomInfo =
  case listingType of
    Ongoing -> [ text "" ]
    Invite ->
      [ p [ class "room-btn-container" ]
        [ a
          [ class "btn green white-text waves-effect waves-light invite-btn"
          , onClick <| AcceptInvitation roomInfo.room
          ]
          [ i [ class "material-icons" ] [ text "check" ]
          , text "Join"
          ]
        ]
      , p [ class "room-btn-container" ]
        [ a [ class "btn red white-text waves-effect waves-light invite-btn"]
          [ i [ class "material-icons" ] [ text "close" ]
          , text "Decline"
          ]
        ]
      ]

viewStartPrivateGameTab : Model -> Html Msg
viewStartPrivateGameTab model =
  div [ class "row"]
    [ div [ class "create-game-tab-container col s12" ]
      [ h5 [ class "game-form-header green-text"] [ text "Create a Game"] ]
    ]


-- Update
update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    UpdateEmail email ->          handleUpdateEmail model email
    UpdateBlurb blurb ->          handleUpdateBlurb model blurb
    UpdateChips ->                handleUpdateChips model
    UpdatePlayer payload ->       handleUpdatePlayer model payload
    UpdateCurrentRooms payload -> handleUpdateCurrentRooms model payload
    AcceptInvitation toRoom ->    handleAcceptInvitation model toRoom
    SubmitEmailUpdate ->          handleSubmitUpdate model Email
    SubmitBlurbUpdate ->          handleSubmitUpdate model Blurb
    HeaderClicked attribute ->    handleHeaderClicked model attribute
    Paginate type_ page_num ->    handlePaginate model type_ page_num
    NewUpdateMessage message ->   handleNewUpdateMessage model message
    NewErrorMessage message ->    handleNewErrorMessage model message
    FBInviteBtnClicked ->         handleFBInviteBtnClicked model
    TabClicked tab ->             handleTabClicked model tab
    ClearUpdateMessage _ ->       handleClearUpdateMessage model
    ClearErrorMessage _ ->        handleClearErrorMessage model
    ConnectedToPlayerChannel ->   ( ( model, Cmd.none ), NoOp )
    SocketOpened ->               ( ( model, Cmd.none ), NoOp )
    SocketClosed ->               ( ( model, Cmd.none ), NoOp )
    SocketClosedAbnormally ->     ( ( model, Cmd.none ), NoOp )

handleUpdateEmail : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateEmail model email =
  let
    profile =
      model.profile
    newProfile =
      { profile | email = email }
  in
  ( ( { model | profile = newProfile }, Cmd.none), NoOp )

handleUpdateBlurb : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleUpdateBlurb model blurb =
  let
    profile =
      model.profile
    newProfile =
      { profile | blurb = blurb }
  in
  ( ( { model | profile = newProfile }, Cmd.none ), NoOp )

handleUpdateChips : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateChips model =
  ( ( model, Cmd.none), NoOp )

handleHeaderClicked : Model -> UpdatableAttribute -> ( ( Model, Cmd Msg), ExternalMsg )
handleHeaderClicked model attribute =
  let
    activeAttribute =
      if model.activeAttribute == attribute then None else attribute
    -- The `Chips` field should not be editable unless the player has 100 chips or fewer
    newActiveAttribute =
      if activeAttribute == Chips && model.player.chips > 100 then None else activeAttribute
  in
  ( ( { model | activeAttribute = newActiveAttribute }, Cmd.none), NoOp )

handleUpdatePlayer : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdatePlayer model payload =
  case Decode.decodeValue Profile.decoder payload of
    Ok newProfile ->
      ( ( { model | profile = newProfile }, Cmd.none), NoOp )
    Err error ->
      ( ( model, Cmd.none), NoOp )

handleUpdateCurrentRooms : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateCurrentRooms model payload =
  let
    newCurrentGames =
      case Decode.decodeValue (Decode.at ["current_rooms"] roomsDecoder) payload of
        Ok newGames -> newGames
        Err _ -> model.currentGames
    newInvitedGames =
      case Decode.decodeValue (Decode.at ["invited_rooms"] roomsDecoder) payload of
        Ok newGames -> newGames
        Err _ -> model.invitedGames
  in
  ( ( { model | currentGames = newCurrentGames, invitedGames = newInvitedGames}, Cmd.none), NoOp )

handleSubmitUpdate : Model -> UpdatableAttribute -> ( ( Model, Cmd Msg), ExternalMsg )
handleSubmitUpdate model attribute =
  let
    cmd =
      case attribute of
        Email -> updatePlayerPush model Email model.profile.email
        Blurb -> updatePlayerPush model Blurb model.profile.blurb
        _ -> Cmd.none
  in
  ( ( model, cmd ), NoOp )

handleNewUpdateMessage : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleNewUpdateMessage model payload =
  case Decode.decodeValue (Decode.at ["message"] Decode.string) payload of
    Ok message ->
      ( ( { model | updateMessages = model.updateMessages ++ [message]
                  , activeAttribute = None }
        , Cmd.none), NoOp )
    Err error -> ( ( model, Cmd.none), NoOp)

handleNewErrorMessage : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleNewErrorMessage model payload =
  case Decode.decodeValue (Decode.at ["error"] Decode.string) payload of
    Ok message ->
      -- The `getPlayerPush` command is used to reset the player to its correct current state
      -- on the server.
      ( ( { model | errorMessages = model.errorMessages ++ [message]}, getPlayerPush model), NoOp )
    Err error -> ( ( model, Cmd.none), NoOp)

handleClearUpdateMessage : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleClearUpdateMessage model =
  case List.tail model.updateMessages of
    Just newList -> ( ( { model | updateMessages = newList}, Cmd.none), NoOp )
    Nothing -> ( ( { model | updateMessages = []}, Cmd.none), NoOp )

handleClearErrorMessage : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleClearErrorMessage model =
  case List.tail model.errorMessages of
    Just newList -> ( ( { model | errorMessages = newList}, Cmd.none), NoOp )
    Nothing -> ( ( { model | errorMessages = []}, Cmd.none), NoOp )

handleFBInviteBtnClicked : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleFBInviteBtnClicked model =
  ( ( model, triggerFBInviteRequest ()), NoOp )

handleTabClicked : Model -> Tab -> ( ( Model, Cmd Msg), ExternalMsg )
handleTabClicked model tab =
  ( ( { model | currentTab = tab }, Cmd.none), NoOp )

handleAcceptInvitation : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleAcceptInvitation model room =
  ( (model, acceptInvitationPush model room), NoOp )

handlePaginate : Model -> String -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handlePaginate model type_ page_num =
  Debug.log "TODO: Implement the handlePaginate function"
  ( ( model, Cmd.none), NoOp )

-- SUBSCRIPTIONS
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions]
    clearMessages =
      case model.updateMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearUpdateMessage
    clearErrors =
      case model.errorMessages of
        [] -> Sub.none
        _ -> Time.every 3000 ClearErrorMessage
  in
  Sub.batch (phoenixSubscriptions ++ [clearMessages, clearErrors])

-- Decoders
roomsDecoder : Decoder Rooms
roomsDecoder =
  decode Rooms
    |> required "rooms" (Decode.list roomInfoDecoder)
    |> required "page" Decode.int
    |> required "total_pages" Decode.int

roomInfoDecoder : Decoder RoomInfo
roomInfoDecoder =
  decode RoomInfo
    |> required "room" Decode.string
    |> required "player_count" Decode.int
    |> required "player_count" (Decode.int |> Decode.andThen (\int -> numToStatus int))

numToStatus : Int -> Decoder RoomStatus
numToStatus number =
  case number of
    0 -> Decode.succeed Empty
    1 -> Decode.succeed WaitingForPlayers
    7 -> Decode.succeed Full
    _ -> Decode.succeed Active

-- Helpers
playerGreeting : Player -> String
playerGreeting player =
  let
    name =
      Player.usernameToString player.username
  in
  name ++ "'s Profile"

styleBodyFor : Model -> UpdatableAttribute -> Html.Attribute Msg
styleBodyFor model attribute =
  let
    displayStyle =
      if model.activeAttribute == attribute then "block" else "none"
  in
  style [ ("display", displayStyle) ]

statusToString : RoomStatus -> String
statusToString status =
  case status of
    WaitingForPlayers -> "Waiting for Players"
    _ -> toString status

paginationConfig : RoomListing -> Pagination.Config Msg
paginationConfig roomListing =
  case roomListing of
    Ongoing -> { onClickMsg = Paginate "current_games" }
    Invite -> { onClickMsg = Paginate "invites" }