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
import Types.Dropdowns as DropdownType exposing (OpenDropdown, DropdownMsg, DropdownItem)
import Types.Page as Page exposing (Page)
import Views.Header as Header exposing (activePageFrom, viewNavBarLinks, navDropdownConfig, navDropdownContext, navLinks)
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
      (wrapWithHeader model False page) <| viewPage model.session False page
    TransitioningFrom page ->
      (wrapWithHeader model True page) <| viewPage model.session True page

wrapWithHeader : Model -> Bool -> Page -> Html Msg -> Html Msg
wrapWithHeader model isLoading page children =
  let
    player =
      model.session.player
    activePage = 
      activePageFrom page
  in  
  div [ class "page-frame"]
    [ nav [ class "teal darken-4 nav-container" ]
      [ div [ class "filler"] [] 
        , div [ class "logo-container" ]
          [ a [ Route.href Route.Home, class "logo" ] [ text "PokerEx"] ]
        , ul [ class "nav-links", class "hide-on-med-and-down" ]
          (viewNavBarLinks activePage)
        , div [ class "filler hide-on-large-only" ] [ Html.map HeaderMsg navDropdownConfig.topLevelHtml ]
      ]
      , Html.map HeaderMsg (Dropdown.view navDropdownConfig (navDropdownContext model) navLinks) 
      , children
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
    Page.NotFound ->
      NotFound.view session

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
        route =
          urlFromString item
      in
      ( { model | openDropdown = DropdownType.AllClosed}, Navigation.modifyUrl route)
    ( _, Page.NotFound ) ->
      ( model, Cmd.none )
    ( _, _ ) ->
      ( model, Cmd.none )

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    subs =
      [ pageSubscriptions (getPage model.pageState)
      , Sub.map SetPlayer sessionChange
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

urlFromString : String -> String
urlFromString string =
  let
    formatted =
      String.toLower string
    prefix =
      "#/"
  in
  case formatted of
    "login" -> prefix ++ formatted
    "signup" -> prefix ++ "register"
    _ -> prefix   

sessionChange : Sub (Maybe Player)
sessionChange =
  Ports.onSessionChange (Decode.decodeValue Player.decoder >> Result.toMaybe)

pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
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

main : Program Value Model Msg
main =
  Navigation.programWithFlags (Route.fromLocation >> SetRoute)
      { init = init
      , view = view
      , subscriptions = subscriptions
      , update = update
      }
