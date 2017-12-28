module Page.Profile exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile)
import Data.AuthToken as AuthToken
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (class, placeholder, classList, style)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Push as Push exposing (Push)
import Phoenix

type alias Model =
  { player : Player
  , profile : Profile
  , activeAttribute : UpdatableAttribute
  , channelSubscriptions : List (Channel Msg)
  }

type Msg
  = UpdateEmail String
  | UpdateChips
  | HeaderClicked UpdatableAttribute
  | UpdatePlayer Decode.Value
  | ConnectedToPlayerChannel
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally

type ExternalMsg
  = NoOp

type UpdatableAttribute
  = Email
  | Chips
  | None

-- INITIALIZATION
initialModel : Player -> Model
initialModel player =
  { player = player
  , profile = profileFor player
  , activeAttribute = None
  , channelSubscriptions = [ playerChannel player ]
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

-- VIEW
view : Session -> Model -> Html Msg
view session model =
  div [ class "profile-container"]
    [ h1 [ class "teal-text profile-greeting" ]
      [ text <| (playerGreeting model.player) ]
    , div [ class "profile-pane-container"]
      [ div [ class "profile-pane" ]
       [ ul [ class "collapsible popout"]
        (viewProfileForm model)
       ]
      , div [ class "profile-pane" ] []
      ]
    ]

viewProfileForm : Model -> List (Html Msg)
viewProfileForm model =
  [ (viewUsernameEditField model)
  , li [ classList [ ("active", model.activeAttribute == Email) ] ]
    [ viewEditHeaderFor Email model
    , viewEditFieldFor Email (UpdateEmail model.profile.email) model
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
        [ form [ onSubmit msg ]
          [ div [ class "input-field" ]
            [ input [ placeholder model.player.email ] [] ]
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
    Chips -> if model.player.chips <= 100 then editHtml else (text "")
    _ -> text ""


-- Update
update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    UpdateEmail email ->        handleUpdateEmail model email
    UpdateChips ->              handleUpdateChips model
    UpdatePlayer payload ->     handleUpdatePlayer model payload
    ConnectedToPlayerChannel -> handleConnectedToPlayerChannel model
    HeaderClicked attribute ->  handleHeaderClicked model attribute
    SocketOpened ->             ( ( model, Cmd.none ), NoOp )
    SocketClosed ->             ( ( model, Cmd.none ), NoOp )
    SocketClosedAbnormally ->   ( ( model, Cmd.none ), NoOp )

handleUpdateEmail : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateEmail model email =
  let
    profile =
      model.profile
    newProfile =
      { profile | email = email }
  in
  ( ( { model | profile = profile }, Cmd.none), NoOp )

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
      Debug.log ("Update player failed with: " ++ (toString error))
      ( ( model, Cmd.none), NoOp )

handleConnectedToPlayerChannel : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleConnectedToPlayerChannel model =
  Debug.log "TODO: Implement handleConnectedToPlayerChannel; Currently falling back to NoOp"
  ( ( model, Cmd.none), NoOp)

-- SUBSCRIPTIONS
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  let
    phoenixSubscriptions =
      [ Phoenix.connect (socket session) model.channelSubscriptions]
  in
  Sub.batch phoenixSubscriptions

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