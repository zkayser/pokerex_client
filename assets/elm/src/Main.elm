module Main exposing (..)

import Data.Session as Session exposing (Session)
import Data.Player as Player exposing (Player)
import Html as Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (onInput, onClick)
import Json.Encode exposing (Value)
import Json.Decode as Decode
import Task
import Navigation exposing (Location)
import Ports
import Route exposing (Route)
import Types.Dropdowns as DropdownType exposing (OpenDropdown)
import Views.Header as Header 
import Views.Helpers as Helpers exposing (ActivePage(..))
import Page.Home as Home
import Page.Errored as Errored exposing (PageLoadError)
import Page.Login as Login
import Page.Register as Register
import Page.NotFound as NotFound
import Widgets.Dropdown as Dropdown
import Mouse

type Msg
 = SetRoute (Maybe Route)
 | HomeLoaded (Result PageLoadError Home.Model)
 | HeaderMsg DropdownMsg
 | HomeMsg Home.Msg
 | LoginMsg Login.Msg 
 | RegisterMsg Register.Msg
 | SetPlayer (Maybe Player)

type Page
  = Blank
  | NotFound
  | Login Login.Model
  | Register Register.Model
  | Home Home.Model

type PageState
  = Loaded Page
  | TransitioningFrom Page

type DropdownMsg
  = Toggle OpenDropdown
  | NavItemPicked String
  | Blur

setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
  let
    transition toMsg task =
      ( { model | pageState = TransitioningFrom (getPage model.pageState) }, Task.attempt toMsg task )
  in
    case maybeRoute of
      Nothing ->
        ( { model | pageState = Loaded NotFound }, Cmd.none)
      Just Route.Login ->
        ( { model | pageState = Loaded (Login Login.initialModel )}, Cmd.none )
      Just Route.Logout ->
        ( model, Cmd.none )
      Just Route.Register ->
        ( { model | pageState = Loaded (Register Register.initialModel )}, Cmd.none )
      Just Route.Home ->
        ( { model | pageState = Loaded (Home Home.initialModel)}, Cmd.none )

type alias Model =
  { session : Session
  , pageState : PageState
  , openDropdown : OpenDropdown
  }

-- INITIALIZATION --

init : Value -> Location -> ( Model, Cmd Msg )
init val location =
  setRoute (Route.fromLocation location)
   { pageState = Loaded initialPage
   , session = { player = decodeUserFromJson val }
   , openDropdown = DropdownType.AllClosed
   }

decodeUserFromJson : Value -> Maybe Player
decodeUserFromJson json =
  json
    |> Decode.decodeValue Decode.string
    |> Result.toMaybe
    |> Maybe.andThen (Decode.decodeString Player.decoder >> Result.toMaybe)

initialPage : Page
initialPage =
  Blank

-- VIEW --

view : Model -> Html Msg
view model =
  case model.pageState of
    Loaded page ->
      (wrapWithHeader model False page) 
      <| viewPage model.session False page
    TransitioningFrom page ->
      (wrapWithHeader model True page) 
      <| viewPage model.session True page

wrapWithHeader : Model -> Bool -> Page -> Html Msg -> Html Msg
wrapWithHeader model isLoading page children =
  let
    player =
      model.session.player
    activePage = 
      activePageFrom page
  in  
  div [ class "page-frame"]
    [ nav [ class "nav-top teal darken-4" ] 
      [ div [ class "nav-wrapper valign-wrapper" ] 
        [ a [ Route.href Route.Home, class "brand-logo center" ] [ text "PokerEx"] 
        , ul [ id "nav-mobile", class "right always-right hide-on-med-and-down" ]
          (viewNavBarLinks activePage)
        , Html.map HeaderMsg navDropdownConfig.topLevelHtml
        , Html.map HeaderMsg (Dropdown.view navDropdownConfig (navDropdownContext model) navLinks)
        ]
      ]
      , children
    ]

viewNavBarLinks : ActivePage -> List (Html Msg)
viewNavBarLinks page =
  [ navBarLink (page == Helpers.Login) Route.Login [ text "Login" ]
  , navBarLink (page == Helpers.Registration) Route.Register [ text "Signup" ] 
  ]

navBarLink : Bool -> Route -> List (Html Msg) -> Html Msg
navBarLink isActive route linkContent =
  li [ classList [ ("active", isActive) ] ]
    [ a [ Route.href route ] linkContent ]

activePageFrom : Page -> ActivePage
activePageFrom page =
  case page of
    Login _ -> Helpers.Login
    Register _ -> Helpers.Registration
    Home _ -> Helpers.Home
    _ -> Helpers.Other

viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
  case page of
    Blank ->
      -- Very first page load while waiting for data via Http
      Html.text ""
    Home subModel ->
      Home.view session subModel
        |> Html.map HomeMsg
    Login subModel ->
      Login.view session subModel
        |> Html.map LoginMsg
    Register subModel ->
      Register.view session subModel
        |> Html.map RegisterMsg
    NotFound ->
      NotFound.view session

-- NavDropdown --
navDropdownConfig : Dropdown.Config DropdownMsg
navDropdownConfig =
  { topLevelHtml = i 
    [  class "material-icons nav-dropdown-btn right always-right hide-on-large-only"
    , onClick (Toggle DropdownType.NavBarDropdown)
    ] [ text "reorder" ]
  , clickedMsg = Toggle DropdownType.NavBarDropdown
  , itemPickedMsg = NavItemPicked
  }

navDropdownContext : Model -> Dropdown.Context
navDropdownContext model =
  { selectedItem = Nothing
  , isOpen = model.openDropdown == DropdownType.NavBarDropdown
  }

navLinks : List String
navLinks =
  [ "Login", "Signup" ]



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
    ( LoginMsg subMsg, Login subModel ) ->
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
      ( { newModel | pageState = Loaded (Login pageModel) }, Cmd.map LoginMsg cmd)
    ( RegisterMsg subMsg, Register subModel) ->
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
      ( { newModel | pageState = Loaded (Register pageModel) }, Cmd.map RegisterMsg cmd)
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
    ( HeaderMsg (Toggle DropdownType.NavBarDropdown), _) ->
      let
        newDropdown =
          if model.openDropdown == DropdownType.AllClosed then
            DropdownType.NavBarDropdown
          else
            DropdownType.AllClosed
      in 
      ( { model | openDropdown = newDropdown }, Cmd.none )
    ( _, NotFound ) ->
      ( model, Cmd.none )
    ( _, _ ) ->
      ( model, Cmd.none )

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ pageSubscriptions (getPage model.pageState)
    , Sub.map SetPlayer sessionChange
    ]

getPage : PageState -> Page
getPage pageState =
  case pageState of
    Loaded page ->
      page
    TransitioningFrom page ->
      page

sessionChange : Sub (Maybe Player)
sessionChange =
  Ports.onSessionChange (Decode.decodeValue Player.decoder >> Result.toMaybe)

pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
  case page of
    Blank ->
      Sub.none
    NotFound ->
      Sub.none
    Login _ ->
      Sub.none
    Register _ ->
      Sub.none
    Home _ ->
      Sub.none

main : Program Value Model Msg
main =
  Navigation.programWithFlags (Route.fromLocation >> SetRoute)
      { init = init
      , view = view
      , subscriptions = (\model -> Sub.none)
      , update = update
      }
