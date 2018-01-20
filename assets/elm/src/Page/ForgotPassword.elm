module Page.ForgotPassword exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)
import Data.PasswordReset as PasswordReset exposing (PasswordReset)
import Http
import Time exposing (Time)
import Validate exposing (..)
import Request.Player as Request
import Widgets.Toast as Toast

type alias Model =
  { email : String, errors : List String, messages : List String }

type Msg
  = SendPasswordReset
  | SubmitForm
  | SetEmail String
  | ServerResult (Result Http.Error PasswordReset)
  | ClearMessage Time
  | ClearError Time

type ExternalMsg = NoOp

initialModel : Model
initialModel =
  { email = "", errors = [], messages = [] }

view : Model -> Html Msg
view model =
  div
    [  ]
    [ h1 [ class "page-title teal-text", style [ ("text-align", "center") ] ] [ text "Forgot Password"]
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
      [ placeholder "Enter your email"
      , type_ "text"
      , value model.email
      , onInput SetEmail
      ]
      []
    , button [ class "btn waves-effect blue", type_ "button", onClick SubmitForm ]
        [ text "Send Password Reset" ]
    ]

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
  case msg of
    SendPasswordReset ->    handleSendPasswordReset model
    SubmitForm ->           handleSubmitForm model
    SetEmail email ->       handleSetEmail model email
    ServerResult result ->  handleServerResult model result
    ClearMessage _ ->       handleClearMessage model
    ClearError _ ->         handleClearError model

handleSendPasswordReset : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSendPasswordReset model =
  ( ( model, Cmd.none), NoOp )

handleSubmitForm : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSubmitForm model =
  case validate model of
    [] -> ( ( { model | errors = [] }, Http.send ServerResult (Request.passwordReset model)), NoOp )
    errors -> ( ( { model | errors = errors }, Cmd.none ), NoOp )

handleSetEmail : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetEmail model email =
  ( ( { model | email = email }, Cmd.none ), NoOp )

handleClearMessage : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleClearMessage model =
  case List.tail model.messages of
    Just newList -> ( ( { model | messages = newList }, Cmd.none), NoOp )
    Nothing -> ( ( { model | messages = []}, Cmd.none), NoOp )

handleClearError : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleClearError model =
  case List.tail model.errors of
    Just newList -> ( ( { model | errors = newList }, Cmd.none), NoOp )
    Nothing -> ( ( { model | errors = []}, Cmd.none), NoOp )

handleServerResult : Model -> (Result Http.Error PasswordReset) -> ( ( Model, Cmd Msg), ExternalMsg)
handleServerResult model result =
  case result of
    Ok passwordReset ->
      let
        newModel =
          case passwordReset.type_ of
            PasswordReset.Success -> { model | messages = passwordReset.message :: model.messages }
            PasswordReset.Error -> { model | errors = passwordReset.message :: model.errors }
      in
      ( ( newModel, Cmd.none), NoOp)
    Err _ ->
      let
        errorMessage =
          "Something went wrong with your request. Please submit the form and try again"
      in
      ( ( { model | errors = errorMessage :: model.errors }, Cmd.none), NoOp)


-- VALIDATION --
validate : Model -> List String
validate =
  Validate.all
    [ .email >> ifBlank ("You must enter an email") ]

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