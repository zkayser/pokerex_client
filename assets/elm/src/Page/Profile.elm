module Page.Profile exposing (..)

import Data.Player as Player exposing (Player, Username)
import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile)
import Data.AuthToken as AuthToken
import Ports exposing (triggerFBInviteRequest)
import Route
import Widgets.Pagination as Pagination exposing (paginate)
import Widgets.Modal as Modal
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
  , newGame : NewGame
  , playerList : PlayerPage
  , playerSearchList : List String
  , startGameSubTab : StartGameSubTab
  , searchQuery : String
  , currentSearchPage : Int
  , openModal : ProfileModal
  }

type alias Rooms = { rooms : RoomInfoList, page : Int, totalPages : Int}

type alias RoomInfo = { room : String, playerCount : Int, status : RoomStatus }

type alias RoomInfoList = List RoomInfo

type alias PlayerPage = { players : List String, page : Int, totalPages : Int}

type alias NewGame = { title : String, owner : String, invitees : List String }

type RoomStatus
  = Full
  | Empty
  | WaitingForPlayers
  | Active

type RoomListing
  = Invite
  | Ongoing

type PageList = OngoingGames | Invites | Players

type ProfileModal = AllClosed | DeleteModal

type Msg
  = UpdateEmail String
  | UpdateBlurb String
  | UpdateSearch String
  | UpdateChips
  | SubmitEmailUpdate
  | SubmitBlurbUpdate
  | UpdatePlayer Decode.Value
  | UpdatePlayerList Decode.Value
  | UpdateCurrentRooms Decode.Value
  | UpdateSearchList Decode.Value
  | AcceptInvitation String
  | HeaderClicked UpdatableAttribute
  | ChangeSubTab StartGameSubTab
  | OpenDeleteConfirmation
  | DeleteProfile
  | CloseModal
  | SubmitCreateGameForm
  | SubmitSearch
  | SetGameTitle String
  | RemoveInvitee String
  | AddInvitee String
  | RoomCreated
  | CreateRoomFailed Decode.Value
  | Paginate String String
  | PaginateSearch String
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

type StartGameSubTab = PlayerList | PlayerSearchList

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
  , newGame = { title = "", owner = "", invitees = []}
  , playerList = { players = [], page = 1, totalPages = 0}
  , playerSearchList = []
  , startGameSubTab = PlayerList
  , searchQuery = ""
  , currentSearchPage = 1
  , openModal = AllClosed
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
    |> Channel.on "player_search_list" (\payload -> UpdateSearchList payload)

privateRoomsChannel : Player -> Channel Msg
privateRoomsChannel player =
  Channel.init ("private_rooms:" ++ (Player.usernameToString player.username))
    |> Channel.on "current_rooms" (\payload -> UpdateCurrentRooms payload)
    |> Channel.on "player_list" (\list -> UpdatePlayerList list)
    |> Channel.on "error" (\error -> NewErrorMessage error)
    |> Channel.on "new_current_rooms" (\payload -> UpdateCurrentRooms payload)
    |> Channel.on "new_invited_rooms" (\payload -> UpdateCurrentRooms payload)
    |> Channel.on "new_players" (\payload -> UpdatePlayerList payload)

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
      Push.init ("players:" ++ (getPlayerName model)) "get_player"
        |> Push.withPayload (Encode.object [])
  in
  Phoenix.push socketUrl push

acceptInvitationPush : Model -> String -> Cmd Msg
acceptInvitationPush model room =
  let
    playerName =
      getPlayerName model
    push =
      Push.init ("private_rooms:" ++ playerName) "accept_invitation"
        |> Push.withPayload (Encode.object
            [ ("player", Encode.string playerName )
            , ("room", Encode.string room)
            ] )
  in
  Phoenix.push socketUrl push

createGamePush : Model -> Cmd Msg
createGamePush model =
  let
    playerName =
      getPlayerName model
    push =
      Push.init ("private_rooms:" ++ playerName) "create_room"
        |> Push.withPayload (encodeNewGame model.newGame)
        |> Push.onOk (\_ -> RoomCreated)
        |> Push.onError (\payload -> CreateRoomFailed payload)
  in
  Phoenix.push socketUrl push

getPage : Model -> PageList -> Int -> Cmd Msg
getPage model pageType pageNum =
  let
    playerName =
      getPlayerName model
    getPageTypeParam =
      case pageType of
        OngoingGames -> "current_rooms"
        Invites -> "invited_rooms"
        Players -> "players"
    push =
      Push.init ("private_rooms:" ++ playerName) "get_page"
        |> Push.withPayload (Encode.object [  ("for", Encode.string getPageTypeParam ),
                                              ("page_num", Encode.int pageNum )
                                            ] )
  in
  Phoenix.push socketUrl push

submitSearchPush : Model -> Cmd Msg
submitSearchPush model =
  let
    playerName =
      getPlayerName model
    push =
      Push.init("players:" ++ playerName) "player_search"
        |> Push.withPayload (Encode.object [ ("query", Encode.string model.searchQuery)])
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
        , div [ class "delete-profile-container" ]
          [ button [ onClick OpenDeleteConfirmation, class "btn red white-text" ]
            [ text "Delete your profile"]
          ]
        ]
      , div [ class "profile-pane" ]
        (viewTabs model)
      ]
    , if model.openModal == DeleteModal then Modal.view <| deleteProfileModalConfig model else text ""
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
         [ paginate model.currentGames (paginationConfig OngoingGames) ]++
          (List.map (viewRoom Ongoing) model.currentGames.rooms)

viewInvitedGames : Model -> List (Html Msg)
viewInvitedGames model =
  case model.invitedGames.rooms of
    [] -> [ h5 [ class "game-info-header purple-text" ] [ text "Your Invites"]
          , p [ class "game-info-text"] [ text "You currently have no invites"]
          , div [ class "row" ]
            [ a
              [ class "game-info-text btn blue white-text waves-effect col s6 offset-s3"
              , onClick (TabClicked StartPrivateGame)
              ]
              [ text "Start a Game"]
            ]
          ]
    _ ->
      [ h5 [ class "game-info-header purple-text"] [ text "Your Invites"] ] ++
      [ paginate model.invitedGames (paginationConfig Invites) ] ++
      (List.map (viewRoom Invite) model.invitedGames.rooms)


viewRoom : RoomListing -> RoomInfo -> Html Msg
viewRoom listingType roomInfo =
  let
    linkAttrs =
      case listingType of
        Ongoing -> [ Route.href (Route.Room "private" roomInfo.room) ]
        _ ->  []
  in
  li [ class <| "collection-item " ++ (toString roomInfo.status) ++ " room-info-item" ]
    [ div [ class "room-list-title" ]
      [ span [ class "teal-text" ]
        [ a linkAttrs [ text <| viewRoomTitle roomInfo.room ] ]
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

viewRoomTitle : String -> String
viewRoomTitle title =
  String.split "_" title
    |> String.join " "

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
      [ h5 [ class "game-form-header green-text"] [ text "Create a Game"]
      , hr [] []
      , form [ onSubmit SubmitCreateGameForm ]
        [ div [ class "input-field"]
          [ input
            [ onInput (\str -> SetGameTitle str)
            , placeholder "Give your game a name!"
            , Attributes.type_ "text"
            , Attributes.value model.newGame.title
            , Attributes.id "game-title"
            ]
            []
          ]
        ]
      , div [ class "player-list-container col s12" ]
        [ h6 [ class "purple-text" ] [ text "Invite up to 6 players" ]
        , ul [ class "collection" ]
          (List.map viewInvitedPlayer (zipNamesWithColors model.newGame.invitees))
        ]
      , viewCreateFormSubTab model
      , div [ class "submit-game-btn-container col s12"]
        [
          a [ class "submit-game-btn btn blue white-text", onClick SubmitCreateGameForm ]
            [ text "Create Game" ]
        ]
      ]
    ]

viewCreateFormSubTab : Model -> Html Msg
viewCreateFormSubTab model =
  let
    renderedList =
      case model.startGameSubTab of
        PlayerSearchList ->
          List.map viewPlayer (zipNamesWithColors <| syncInviteesAndPlayers model (searchListForPage model))
        _ ->
          List.map viewPlayer (zipNamesWithColors <| syncInviteesAndPlayers model model.playerList.players)
    pagination =
      case model.startGameSubTab of
        PlayerList -> paginate model.playerList (paginationConfig Players)
        PlayerSearchList -> paginate (paginationObjectForSearch model) paginationConfigSearch
  in
  div [ class "player-list-container col s12" ]
      [ h6 [ class "teal-text" ] [ text "Choose other players to invite"]
      , viewSubTabs model
      , ul [ class "collection player-list-collection" ]
        ( [ viewPlayerSearchForm model ] ++
          [ pagination ] ++
        renderedList )
      ]

viewSubTabs : Model -> Html Msg
viewSubTabs model =
  div [ class "row" ]
    [ div [ class "col s6 offset-s3" ]
      [ a [ class "waves-effect waves-teal btn-flat sub-tab"
          , classList [ ("active-sub-tab", model.startGameSubTab == PlayerList) ]
          , onClick <| ChangeSubTab PlayerList
          ]
          [ text "Browse Players" ]
      , a [ class "waves-effect waves-teal btn-flat sub-tab"
          , classList [ ("active-sub-tab", model.startGameSubTab == PlayerSearchList) ]
          , onClick <| ChangeSubTab PlayerSearchList
          ]
          [ text "Search"]
      ]
    ]

viewInvitedPlayer : (String, String) -> Html Msg
viewInvitedPlayer (iconColor, player) =
  li [ class "collection-item avatar player-list-elem" ]
    [ i [ class <| "material-icons large " ++ iconColor ] [ text "person" ]
    , span [ class "title" ] [ text player ]
    , a [ class "btn btn-floating red white-text", onClick (RemoveInvitee player) ]
      [ i [ class "material-icons"] [ text "close" ] ]
    ]

viewPlayer : (String, String) -> Html Msg
viewPlayer (iconColor, player) =
  li [ class "collection-item avatar player-list-elem" ]
    [ i [ class <| "material-icons large " ++ iconColor ] [ text "person" ]
    , span [ class "title" ] [ text player ]
    , a [ class "btn btn-floating green white-text", onClick (AddInvitee player) ]
      [ i [ class "material-icons"] [ text "check"]]
    ]

viewPlayerSearchForm : Model -> Html Msg
viewPlayerSearchForm model =
  div [ class "row" ]
    [ form [ class "col s12", onSubmit SubmitSearch ]
      [ div [ class "row" ]
        [ div [ class "input-field col s6 offset-s3" ]
          [ i [ class "material-icons prefix teal-text", onClick SubmitSearch ] [ text "search" ]
          , input
            [ Attributes.type_ "text"
            , Attributes.value model.searchQuery
            , Attributes.placeholder "Search"
            , Attributes.id "player-search-query"
            , onInput (\str -> UpdateSearch str)
            ]
            []
          ]
        ]
      ]
    ]

viewDeleteProfileModal : Model -> List (Html Msg)
viewDeleteProfileModal model =
  [
    h1 [ class "red-text" ] [ text "Are you sure?" ]
  , div [ class "row" ]
    [ div [ class "col s3 offset-s3" ]
      [ button [ class "btn red white-text", onClick DeleteProfile ] [ text "Okay" ] ]
    , div [ class "col s3"]
      [ button [ class "btn-flat waves-effect teal-text", onClick CloseModal ] [ text "Cancel" ] ]
    ]
  ]

-- Update
update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    UpdateEmail email ->          handleUpdateEmail model email
    UpdateBlurb blurb ->          handleUpdateBlurb model blurb
    UpdateSearch query ->         handleUpdateSearch model query
    UpdateChips ->                handleUpdateChips model
    UpdatePlayer payload ->       handleUpdatePlayer model payload
    UpdatePlayerList payload ->   handleUpdatePlayerList model payload
    UpdateCurrentRooms payload -> handleUpdateCurrentRooms model payload
    UpdateSearchList payload ->   handleUpdateSearchList model payload
    AcceptInvitation toRoom ->    handleAcceptInvitation model toRoom
    SubmitEmailUpdate ->          handleSubmitUpdate model Email
    SubmitBlurbUpdate ->          handleSubmitUpdate model Blurb
    HeaderClicked attribute ->    handleHeaderClicked model attribute
    ChangeSubTab newSubTab ->     handleChangeSubTab model newSubTab
    OpenDeleteConfirmation ->     handleOpenDeleteConfirmation model
    DeleteProfile ->              handleDeleteProfile model
    CloseModal ->                 handleCloseModal model
    Paginate type_ page_num ->    handlePaginate model type_ page_num
    PaginateSearch page_num ->    handlePaginateSearch model page_num
    SubmitCreateGameForm ->       handleSubmitCreateGameForm model
    SubmitSearch ->               handleSubmitSearch model
    RoomCreated ->                handleRoomCreated model
    CreateRoomFailed payload ->   handleCreateRoomFailed model payload
    SetGameTitle title ->         handleSetGameTitle model title
    RemoveInvitee invitee ->      handleRemoveInvitee model invitee
    AddInvitee invitee ->         handleAddInvitee model invitee
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

handleUpdateSearch : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleUpdateSearch model query =
  ( ( { model | searchQuery = query }, Cmd.none), NoOp )

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

handleUpdatePlayerList : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdatePlayerList model payload =
  case Decode.decodeValue playerListDecoder payload of
    Ok newPlayerPage ->
      ( ( { model | playerList = newPlayerPage }, Cmd.none), NoOp )
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

handleUpdateSearchList : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateSearchList model payload =
  let
    newSearchList =
      case Decode.decodeValue (Decode.at ["players"] (Decode.list Decode.string)) payload of
        Ok players -> players
        Err _ -> model.playerSearchList
  in
  ( ( { model | playerSearchList = newSearchList }, Cmd.none), NoOp )

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
      -- based on its state on the server.
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

handleChangeSubTab : Model -> StartGameSubTab -> ( ( Model, Cmd Msg ), ExternalMsg )
handleChangeSubTab model subTab =
  ( ( { model | startGameSubTab = subTab }, Cmd.none), NoOp )

handleAcceptInvitation : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleAcceptInvitation model room =
  ( (model, acceptInvitationPush model room), NoOp )

handleOpenDeleteConfirmation : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleOpenDeleteConfirmation model =
  ( ( { model | openModal = DeleteModal }, Cmd.none), NoOp )

handlePaginate : Model -> String -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handlePaginate model type_ pageNum =
  let
    page =
      case String.toInt pageNum of
        Ok num -> num
        Err _ -> 1
    listing =
      case type_ of
        "current_rooms" -> OngoingGames
        "invited_rooms" -> Invites
        "players" -> Players
        _ -> Players
  in
  ( ( model, getPage model listing page), NoOp )

handlePaginateSearch : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handlePaginateSearch model page_num =
  let
    newPage =
      case String.toInt page_num of
        Ok page -> page
        Err _ -> 1
  in
  ( ( { model | currentSearchPage = newPage }, Cmd.none ), NoOp )

handleSubmitCreateGameForm : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleSubmitCreateGameForm model =
  let
    owner =
      String.trim (Player.usernameToString model.player.username)
    game =
      model.newGame
    newGame =
      { game | owner = owner }
    isInvalid =
      ((String.isEmpty newGame.title), (List.isEmpty newGame.invitees))
    errorMsgs =
      case isInvalid of
        (True, True) -> [emptyTitleErrorMsg, noInviteesErrorMsg]
        (True, _) -> [emptyTitleErrorMsg]
        (_, True) -> [noInviteesErrorMsg]
        _ -> []
    newModel =
      case isInvalid of
        (True, True) -> { model | errorMessages = errorMsgs ++ model.errorMessages }
        (True, _) -> { model | errorMessages = errorMsgs ++ model.errorMessages }
        (_, True) -> { model | errorMessages = errorMsgs ++ model.errorMessages }
        (False, False) -> { model | newGame = newGame}
    cmd =
      case isInvalid of
        (True, True) -> Cmd.none
        (_, True) -> Cmd.none
        (True, _) -> Cmd.none
        (False, False) -> createGamePush newModel
  in
  ( ( newModel, cmd ), NoOp)

handleSubmitSearch : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleSubmitSearch model =
  let
    (subTab, cmd) =
      case String.isEmpty model.searchQuery of
        True -> (model.startGameSubTab, Cmd.none)
        False -> (PlayerSearchList, submitSearchPush model)
  in
  ( ( { model | startGameSubTab = subTab }, cmd), NoOp )

handleSetGameTitle : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleSetGameTitle model title =
  let
    game =
      model.newGame
    newGame =
      { game | title = title }
  in
  ( ( { model | newGame = newGame }, Cmd.none), NoOp )

handleRemoveInvitee : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleRemoveInvitee model invitee =
  let
    game =
      model.newGame
    invitees =
      game.invitees
    newInvitees =
      List.filter (\name -> name /= invitee) invitees
    newGame =
      { game | invitees = newInvitees }
  in
  ( ( { model | newGame = newGame }, Cmd.none), NoOp )

handleAddInvitee : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleAddInvitee model name =
  let
    game =
      model.newGame
    newInvitees =
      case List.length game.invitees < 6 of
        True -> name :: game.invitees
        False -> game.invitees
    newGame =
      { game | invitees = newInvitees }
    newErrorMessages =
      case List.length game.invitees >= 6 of
        True -> "You can only invite up to 6 players" :: model.errorMessages
        False -> model.errorMessages
  in
  ( ( { model | newGame = newGame, errorMessages = newErrorMessages }, Cmd.none), NoOp )

handleRoomCreated : Model -> ( ( Model, Cmd Msg), ExternalMsg)
handleRoomCreated model =
  let
    newMessages =
      "Room successfully created" :: model.updateMessages
    newGame =
      { title = "", owner = Player.usernameToString model.player.username, invitees = []}
  in
  ( ({ model | newGame = newGame, updateMessages = newMessages}, Cmd.none), NoOp )

handleCreateRoomFailed : Model -> Decode.Value -> ( ( Model, Cmd Msg), ExternalMsg)
handleCreateRoomFailed model payload =
  let
    newMessages =
      "An error occurred" :: model.errorMessages
  in
  ( ( { model | errorMessages = newMessages }, Cmd.none), NoOp )

handleDeleteProfile : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleDeleteProfile model =
  -- TODO: Push a message to the server to delete the user's profile
  -- and redirect the user to the home route
  ( ( model, Cmd.none), NoOp )

handleCloseModal : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleCloseModal model =
  ( ( { model | openModal = AllClosed }, Cmd.none), NoOp )


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

playerListDecoder : Decoder PlayerPage
playerListDecoder =
  decode PlayerPage
    |> required "players" (Decode.list Decode.string)
    |> required "page" Decode.int
    |> required "total_pages" Decode.int

-- Encoders
encodeNewGame : NewGame -> Encode.Value
encodeNewGame newGame =
  Encode.object [ ("title", Encode.string newGame.title)
                , ("owner", Encode.string newGame.owner)
                , ("invitees", Encode.list (List.map Encode.string newGame.invitees))
                ]

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

paginationConfig : PageList -> Pagination.Config Msg
paginationConfig roomListing =
  case roomListing of
    OngoingGames -> { onClickMsg = Paginate "current_games", linksToShow = 5 }
    Invites -> { onClickMsg = Paginate "invites", linksToShow = 5 }
    Players -> { onClickMsg = Paginate "players", linksToShow = 5 }

paginationConfigSearch : Pagination.Config Msg
paginationConfigSearch =
  { onClickMsg = PaginateSearch, linksToShow = 5 }

paginationObjectForSearch : Model -> { totalPages : Int, page : Int }
paginationObjectForSearch model =
  let
    totalPages =
        ((List.length model.playerSearchList) // 10) + 1
  in
  { totalPages = totalPages, page = model.currentSearchPage }

deleteProfileModalConfig : Model -> Modal.Config Msg
deleteProfileModalConfig model =
  { classes = [ "center-align" ]
  , contentHtml = viewDeleteProfileModal model
  , styles = Nothing
  }

syncInviteesAndPlayers : Model -> List String -> List String
syncInviteesAndPlayers model playerList =
  let
    invitees =
      model.newGame.invitees
  in
  List.filter (\player -> not <| (List.member player invitees)) playerList

zipNamesWithColors : List String -> List (String, String)
zipNamesWithColors names =
  let
    size =
      List.length names
    overSizedColorList =
      List.repeat size listColors |> List.concat
    colorList =
      List.take size overSizedColorList
  in
  zip colorList names

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xBack, y :: yBack ) ->
        (x,y) :: zip xBack yBack
    (_, _) ->
        []

listColors : List String
listColors =
  ["blue-text", "green-text", "purple-text", "red-text", "yellow-text", "teal-text"]

emptyTitleErrorMsg : String
emptyTitleErrorMsg =
  "You must enter a title"

noInviteesErrorMsg : String
noInviteesErrorMsg =
  "You must invite at least one player"

getPlayerName : Model -> String
getPlayerName model =
  Player.usernameToString model.player.username

searchListForPage : Model -> List String
searchListForPage model =
  if List.length model.playerSearchList <= 10 then
    model.playerSearchList
  else
    List.drop (10 * (model.currentSearchPage - 1)) model.playerSearchList
    |> List.take 10