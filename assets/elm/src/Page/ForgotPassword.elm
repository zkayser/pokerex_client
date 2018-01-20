module Page.ForgotPassword exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)

type alias Model =
  { email : String }

type Msg = SendPasswordReset | SubmitForm | SetEmail String
type ExternalMsg = NoOp

initialModel : Model
initialModel =
  { email = "" }

view : Model -> Html Msg
view model =
  div
    [  ]
    [ h1 [ class "page-title teal-text", style [ ("text-align", "center") ] ] [ text "Forgot Password"]
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
    SendPasswordReset -> handleSendPasswordReset model
    SubmitForm ->        handleSubmitForm model
    SetEmail email ->    handleSetEmail model email

handleSendPasswordReset : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSendPasswordReset model =
  ( ( model, Cmd.none), NoOp )

handleSubmitForm : Model -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSubmitForm model =
  ( ( model, Cmd.none ), NoOp )

handleSetEmail : Model -> String -> ( ( Model, Cmd Msg ), ExternalMsg )
handleSetEmail model email =
  ( ( { model | email = email }, Cmd.none ), NoOp )