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
import Views.Page as Page exposing (ActivePage)
import Page.Login as Login
import Page.NotFound as NotFound

type Msg
 = SetRoute (Maybe Route)
 | LoginMsg Login.Msg
 | SetPlayer (Maybe Player)

type Page
  = Blank
  | NotFound
  | Login Login.Model

type PageState
  = Loaded Page
  | TransitioningFrom Page

-- Dummy implementation for bootstrapping purposes
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
        ( model, Cmd.none ) -- for now)
      Just Route.Home ->
        ( model, Cmd.none )

type alias Model =
  { session : Session
  , pageState : PageState
  }

-- INITIALIZATION --


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
  setRoute (Route.fromLocation location)
   { pageState = Loaded initialPage
   , session = { player = decodeUserFromJson val }
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
      viewPage model.session False page
    TransitioningFrom page ->
      viewPage model.session True page

viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
  let
    frame =
      Page.frame isLoading session.player
  in
  case page of
    Blank ->
      -- Very first page load while waiting for data via Http
      Html.text ""
        |> frame Page.Other
    Login subModel ->
      Login.view session subModel
        |> frame Page.Other
        |> Html.map LoginMsg
    NotFound ->
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

main : Program Value Model Msg
main =
  Navigation.programWithFlags (Route.fromLocation >> SetRoute)
      { init = init
      , view = view
      , subscriptions = (\model -> Sub.none)
      , update = update
      }
