module Page.ResetPassword exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Time exposing (Time)
import Route
import Data.Player as Player exposing (Player)
import Data.Configuration as Configuration exposing (Configuration)
import Request.Player as Request
import Widgets.Toast as Toast

type alias Model =
  { resetToken : String
  , newPassword : String
  , errors : List String
  , messages : List String
  , apiUrl : String
  }

type Msg
  = SetPassword String
  | SubmitForm
  | ClearMessage Time
  | ClearError Time
  | ServerResult (Result Http.Error Player)

type ExternalMsg = NoOp | SetPlayer Player

initialModel : String -> Configuration -> Model
initialModel resetToken envConfig =
  { resetToken = resetToken
  , newPassword = ""
  , errors = []
  , messages = []
  , apiUrl = envConfig.apiUrl
  }

view : Model -> Html Msg
view model =
  div
    [  ]
    [ h1 [ class "page-title teal-text", style [ ("text-align", "center") ] ] [ text "Reset Password"]
    , Toast.viewMessages model
    , div [ class "auth-page", style [ ("text-align", "center") ] ]
      [ div [ class "auth-form card-panel z-depth-4 rounded" ]
        [ viewForm model ]
      ]
    ]

viewForm : Model -> Html Msg
viewForm model =
  Html.form [ onSubmit SubmitForm ]
    [ input
      [ placeholder "Enter your new password"
      , type_ "password"
      , value model.newPassword
      , onInput SetPassword
      ]
      []
    , button [ class "btn waves-effect blue", type_ "button", onClick SubmitForm ]
        [ text "Reset Password" ]
    ]

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
  case msg of
    SetPassword password -> handleSetPassword model password
    SubmitForm ->           handleSubmitForm model
    ClearError _ ->         handleClearError model
    ClearMessage _ ->       handleClearMessage model
    ServerResult result ->  handleServerResult model result

handleSetPassword : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetPassword model password =
  ( ( { model | newPassword = password }, Cmd.none), NoOp )

handleSubmitForm : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSubmitForm model =
  let
    (newModel, cmd) =
      case String.length model.newPassword >= 6 of
        True -> (model, Http.send ServerResult (Request.resetPassword model))
        False -> ({ model | errors = "Password must be at least 6 characters" :: model.errors }, Cmd.none)
  in
  ( ( newModel, cmd ), NoOp )

handleClearMessage : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleClearMessage model =
  case List.tail model.messages of
    Just newList -> ( ( { model | messages = newList }, Cmd.none ), NoOp )
    Nothing -> ( ( { model | messages = []}, Cmd.none ), NoOp )

handleClearError : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleClearError model =
  case List.tail model.errors of
    Just newList -> ( ( { model | errors = newList }, Cmd.none ), NoOp )
    Nothing -> ( ( { model | errors = [] }, Cmd.none ), NoOp )

handleServerResult : Model -> (Result Http.Error Player) -> ( ( Model, Cmd Msg ), ExternalMsg )
handleServerResult model result =
  case result of
    (Ok player) ->
      ( ( model, Cmd.batch [ Request.storeSession player, Route.modifyUrl Route.Home ] ), SetPlayer player )
    (Err reason) ->
      let
        errorMessage =
          "Your request failed. Your reset token may have expired, or you may have an invalid token." ++
            " Try initiating a new password reset request from the login page and try again."
      in
      ( ( { model | errors = errorMessage :: model.errors}, Cmd.none ), NoOp )

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    clearMessages =
      case model.messages of
        [] -> Sub.none
        _ -> Time.every 5000 ClearMessage
    clearErrors =
      case model.errors of
        [] -> Sub.none
        _ -> Time.every 5000 ClearError
  in
  Sub.batch [clearErrors, clearMessages]