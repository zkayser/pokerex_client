module Main exposing (..)

import Data.Session as Session exposing (Session)
import Data.Player as Player exposing (Player)
import Data.Facebook as Facebook
import Request.Player
import Html as Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (onInput, onClick)
import Json.Encode exposing (Value)
import Json.Decode as Decode
import Http
import Task
import Navigation exposing (Location)
import Ports
import Route exposing (Route)
import Types.Dropdowns as DropdownType exposing (OpenDropdown, DropdownMsg, DropdownItem, DropdownNavbarLink)
import Types.Page as Page exposing (Page)
import Views.Header as Header exposing (activePageFrom, viewNavBarLinks, navDropdownConfig, navDropdownContext, navLinks)
import Views.Helpers as Helpers exposing (ActivePage(..))
import Views.Footer as Footer
import Page.Home as Home
import Page.Errored as Errored exposing (PageLoadError)
import Page.Login as Login
import Page.Register as Register
import Page.Room as Room
import Page.Rooms as Rooms
import Page.Profile as Profile
import Page.NotFound as NotFound
import Page.ForgotPassword as ForgotPassword
import Widgets.Dropdown as Dropdown
import Mouse

type Msg
 = SetRoute (Maybe Route)
 | HomeLoaded (Result PageLoadError Home.Model)
 | HeaderMsg DropdownMsg
 | HomeMsg Home.Msg
 | LoginMsg Login.Msg
 | Logout
 | RegisterMsg Register.Msg
 | RoomsMsg Rooms.Msg
 | RoomMsg Room.Msg
 | ProfileMsg Profile.Msg
 | ForgotPasswordMsg ForgotPassword.Msg
 | SetPlayer (Maybe Player)
 | FBLogin (Result String Facebook.FBData)
 | SentLogin (Result Http.Error Player)

type PageState
  = Loaded Page
  | TransitioningFrom Page

setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
  let
    transition toMsg task =
      ( { model | pageState = TransitioningFrom (getPage model.pageState) }, Task.attempt toMsg task )
  in
    case maybeRoute of
      Nothing ->
        ( { model | pageState = Loaded Page.NotFound }, Cmd.none)
      Just Route.Login ->
        ( { model | pageState = Loaded (Page.Login Login.initialModel )}, Cmd.none )
      Just Route.Logout ->
        ( model, Cmd.none )
      Just Route.Register ->
        ( { model | pageState = Loaded (Page.Register Register.initialModel )}, Cmd.none )
      Just Route.Home ->
        ( { model | pageState = Loaded (Page.Home Home.initialModel)}, Cmd.none )
      Just Route.Rooms ->
        ( { model | pageState = Loaded (Page.Rooms Rooms.initialModel)}, Cmd.none )
      Just (Route.Room roomType roomTitle) ->
        let
          page =
            case model.session.player of
              Just player -> Page.Room (Room.initialModel player roomTitle roomType)
              Nothing -> Page.NotFound
        in
        ( { model | pageState = Loaded (page)}, Cmd.none )
      Just (Route.Profile user) ->
        let
          page =
            case model.session.player of
              Just player -> Page.Profile (Profile.initialModel player)
              Nothing -> Page.NotFound
        in
        ( { model | pageState = Loaded (page)}, Cmd.none )
      Just Route.ForgotPassword ->
        ( { model | pageState = Loaded (Page.ForgotPassword ForgotPassword.initialModel)}, Cmd.none)

type alias Model =
  { session : Session
  , pageState : PageState
  , openDropdown : OpenDropdown
  , selectedItem : DropdownItem
  }

-- INITIALIZATION --

init : Value -> Location -> ( Model, Cmd Msg )
init val location =
  setRoute (Route.fromLocation location)
   { pageState = Loaded initialPage
   , session = { player = decodeUserFromJson val }
   , openDropdown = DropdownType.AllClosed
   , selectedItem = DropdownType.None
   }

decodeUserFromJson : Value -> Maybe Player
decodeUserFromJson json =
  json
    |> Decode.decodeValue Decode.string
    |> Result.toMaybe
    |> Maybe.andThen (Decode.decodeString Player.decoder >> Result.toMaybe)

initialPage : Page
initialPage =
  Page.Blank

-- VIEW --
view : Model -> Html Msg
view model =
  case model.pageState of
    Loaded page ->
      (frame model False page) <| viewPage model.session False page
    TransitioningFrom page ->
      (frame model True page) <| viewPage model.session True page

frame : Model -> Bool -> Page -> Html Msg -> Html Msg
frame model isLoading page children =
  let
    player =
      model.session.player
    activePage =
      activePageFrom page
  in
  div [ class "page-frame"]
    [ header []
      [ nav [ class "teal darken-4 nav-container" ]
          [ div [ class "filler"] []
            , div [ class "logo-container" ]
              [ a [ Route.href Route.Home, class "logo" ] [ text "PokerEx"] ]
            , ul [ class "nav-links", class "hide-on-med-and-down" ]
              (viewNavBarLinks Logout model.session activePage)
            , div [ class "filler hide-on-large-only" ] [ Html.map HeaderMsg navDropdownConfig.topLevelHtml ]
        ]
      ]
      , Html.map HeaderMsg (Dropdown.view navDropdownConfig (navDropdownContext model) (navLinks model.session))
      , children
      , if activePage == Helpers.Home then Footer.view else text ""
    ]

viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
  case page of
    Page.Blank ->
      Html.text ""
    Page.Home subModel ->
      Home.view session subModel
        |> Html.map HomeMsg
    Page.Login subModel ->
      Login.view session subModel
        |> Html.map LoginMsg
    Page.Register subModel ->
      Register.view session subModel
        |> Html.map RegisterMsg
    Page.Rooms subModel ->
      Rooms.view session subModel
        |> Html.map RoomsMsg
    Page.Room subModel ->
      Room.view session subModel
        |> Html.map RoomMsg
    Page.Profile subModel ->
      Profile.view session subModel
        |> Html.map ProfileMsg
    Page.NotFound ->
      NotFound.view session
    Page.ForgotPassword subModel ->
      ForgotPassword.view subModel
        |> Html.map ForgotPasswordMsg

-- UPDATE --
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  updatePage (getPage model.pageState) msg model

updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
  let
    session =
      model.session

    toPage toModel toMsg subUpdate subMsg subModel =
      let
        ( newModel, newCmd ) =
          subUpdate subMsg subModel
      in
      ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
  in
  case ( msg, page ) of
    ( SetRoute route, _ ) ->
      setRoute route model
    ( LoginMsg subMsg, Page.Login subModel ) ->
      let
        ( ( pageModel, cmd ), msgFromPage ) =
          Login.update subMsg subModel

        newModel =
          case msgFromPage of
            Login.NoOp ->
              model
            Login.SetPlayer player ->
              let
                session =
                  model.session
              in
              { model | session = { player = Just player }}
      in
      ( { newModel | pageState = Loaded (Page.Login pageModel) }, Cmd.map LoginMsg cmd)
    ( RegisterMsg subMsg, Page.Register subModel) ->
      let
        ( ( pageModel, cmd), msgFromPage ) =
          Register.update subMsg subModel

        newModel =
          case msgFromPage of
            Register.NoOp ->
              model
            Register.SetPlayer player ->
              let
                session = model.session
              in
              { model | session = { player = Just player}}
      in
      ( { newModel | pageState = Loaded (Page.Register pageModel) }, Cmd.map RegisterMsg cmd)
    ( RoomMsg subMsg, Page.Room subModel ) ->
      let
        ( (roomModel, cmd), msgFromPage ) =
          Room.update subMsg subModel
      in
      ( { model | pageState = Loaded (Page.Room roomModel) }, Cmd.map RoomMsg cmd)
    ( RoomsMsg subMsg, Page.Rooms subModel ) ->
      let
        ( (roomsModel, cmd), msgFromPage ) =
          Rooms.update subMsg subModel
      in
      ( { model | pageState = Loaded (Page.Rooms roomsModel) }, Cmd.map RoomsMsg cmd)
    ( ProfileMsg subMsg, Page.Profile subModel) ->
      let
        ( ( profileModel, cmd), msgsFromPage ) =
          Profile.update subMsg subModel
        newModel =
          case msgsFromPage of
            Profile.NoOp ->
              model
            Profile.Deleted ->
              { model | session = { player = Nothing }}
      in
      ( { newModel | pageState = Loaded (Page.Profile profileModel) }, Cmd.map ProfileMsg cmd)
    ( ForgotPasswordMsg subMsg, Page.ForgotPassword subModel ) ->
      let
        ( (forgotPwdModel, cmd), msgFromPage) =
          ForgotPassword.update subMsg subModel
      in
      ( { model | pageState = Loaded (Page.ForgotPassword forgotPwdModel ) }, Cmd.map ForgotPasswordMsg cmd)
    ( SetPlayer player, _ ) ->
      let
        session =
          model.session
        cmd =
          if session.player /= Nothing && player == Nothing then
            Route.modifyUrl Route.Home
          else
            Cmd.none
      in
      ( { model | session = { session | player = player }}, cmd)
    ( FBLogin result, _ ) ->
      let
        cmd =
          case result of
            (Ok fbData) -> Http.send SentLogin (Request.Player.facebookLogin <| Facebook.encode fbData)
            _ -> Cmd.none
      in
      ( model, cmd )
    ( SentLogin (Ok player), _ ) ->
      ( { model | session = { session | player = Just player }},
          Cmd.batch [ Request.Player.storeSession player, Route.modifyUrl Route.Home ])
    ( SentLogin (Err payload), _) ->
      Debug.log ("Got bad payload: " ++ (toString payload))
      ( model, Cmd.none )
    ( Logout, _ ) ->
      let
        session =
          model.session
        cmds =
          Cmd.batch [ Ports.logout (), Route.modifyUrl Route.Home ]
      in
      ( { model | session = { session | player = Nothing }}, cmds)
    ( HeaderMsg (DropdownType.Toggle DropdownType.NavBarDropdown), _) ->
      let
        newDropdown =
          if model.openDropdown == DropdownType.AllClosed then
            DropdownType.NavBarDropdown
          else
            DropdownType.AllClosed
      in
      ( { model | openDropdown = newDropdown }, Cmd.none )
    ( HeaderMsg (DropdownType.Blur), _ ) ->
      ( { model | openDropdown = DropdownType.AllClosed }, Cmd.none )
    ( HeaderMsg (DropdownType.NavItemPicked item), _ ) ->
      let
        session =
          model.session
        route =
          urlFromString item model -- TODO: Rename urlFromString to reflect new implementation
        actionCmd =
          case item of
            DropdownType.Logout -> Cmd.batch [ Ports.logout (), Route.modifyUrl Route.Home ]
            _ -> Navigation.modifyUrl route
        newSession =
          case item of
            DropdownType.Logout -> { session | player = Nothing }
            _ -> session
      in
      ( { model | session = newSession, openDropdown = DropdownType.AllClosed}, actionCmd)
    ( _, Page.NotFound ) ->
      ( model, Cmd.none )
    ( _, _ ) ->
      ( model, Cmd.none )

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    subs =
      [ pageSubscriptions (getPage model.pageState) model.session
      , Sub.map SetPlayer sessionChange
      , Sub.map FBLogin fbLogin
      ]
    withBlur =
      case model.openDropdown of
        DropdownType.AllClosed -> []
        _ -> [ Sub.map HeaderMsg (Mouse.clicks (always DropdownType.Blur)) ]
    subsWithBlur =
      subs ++ withBlur
  in
  Sub.batch subsWithBlur

getPage : PageState -> Page
getPage pageState =
  case pageState of
    Loaded page ->
      page
    TransitioningFrom page ->
      page

urlFromString : DropdownNavbarLink -> Model -> String
urlFromString navbarLink model =
  let
    urlSuffix =
      case navbarLink of
        DropdownType.Logout -> ""
        DropdownType.Login -> "login"
        DropdownType.Register -> "register"
        DropdownType.Room -> "rooms/public/room_1"
        DropdownType.Rooms -> "rooms"
        DropdownType.Profile -> "profile"
    prefix =
      "#/"
    player =
      case model.session.player of
        Just player -> "/" ++ (Player.usernameToString player.username)
        Nothing -> ""
  in
  case urlSuffix of
    "profile" -> if player == "" then prefix else prefix ++ urlSuffix ++ player
    _ -> prefix ++ urlSuffix

sessionChange : Sub (Maybe Player)
sessionChange =
  Ports.onSessionChange (Decode.decodeValue Player.decoder >> Result.toMaybe)

fbLogin : Sub (Result String Facebook.FBData)
fbLogin =
  Ports.onFBLogin (Decode.decodeValue Facebook.decoder)

pageSubscriptions : Page -> Session -> Sub Msg
pageSubscriptions page session =
  case page of
    Page.Blank ->
      Sub.none
    Page.NotFound ->
      Sub.none
    Page.Login _ ->
      Sub.none
    Page.Register _ ->
      Sub.none
    Page.Home _ ->
      Sub.none
    Page.Rooms subModel ->
      Sub.map RoomsMsg <| Rooms.subscriptions subModel session
    Page.Room subModel ->
      Sub.map RoomMsg <| Room.subscriptions subModel session
    Page.Profile subModel ->
      Sub.map ProfileMsg <| Profile.subscriptions subModel session
    Page.ForgotPassword _ ->
      Sub.none

main : Program Value Model Msg
main =
  Navigation.programWithFlags (Route.fromLocation >> SetRoute)
      { init = init
      , view = view
      , subscriptions = subscriptions
      , update = update
      }
