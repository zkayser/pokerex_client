module Page.Login exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session)
import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Request.Player exposing (storeSession)
import Views.Form as Form
import Validate exposing (..)
import Route


-- Model --

type alias Model =
  { errors : List Error
  , username : String
  , password : String
  }

type alias Error =
  ( Field, String )

initialModel : Model
initialModel =
  { errors = []
  , username = ""
  , password = ""
  }

-- View --

view : Session -> Model -> Html Msg
view session model =
  div [ class "auth-page", style [ ("text-align", "center") ] ]
    [ Form.viewErrors model.errors
    , viewForm
    ]

viewForm : Html Msg
viewForm =
  Html.form [ onSubmit SubmitForm ]
    [ input
      [ placeholder "Username"
      , type_ "text"
      , onInput SetUsername
      ]
      []
    , input
      [ placeholder "Password"
      , type_ "password"
      , onInput SetPassword
      ]
      []
    , button [ class "btn waves-effect blue" ]
        [ text "Login" ]
    ]

-- Update --

type Msg
  = SubmitForm
  | SetUsername String
  | SetPassword String
  | LoginCompleted (Result Http.Error Player)

type ExternalMsg
  = NoOp
  | SetPlayer Player

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    SubmitForm ->
      case validate model of
        [] ->
          ( ( { model | errors = [] }, Http.send LoginCompleted (Request.Player.login model) ), NoOp )
        errors ->
          ( ( { model | errors = errors }, Cmd.none), NoOp )
    SetUsername name ->
      ( ( { model | username = name }, Cmd.none), NoOp )
    SetPassword pass ->
      ( ( { model | password = pass }, Cmd.none), NoOp )
    LoginCompleted (Err error) ->
      let
        errorMessages =
          case error of
            Http.BadStatus response ->
              response.body
                |> decodeString (field "errors" errorsDecoder)
                |> Result.withDefault []
            _ ->
              ["unable to process login"]
      in
      ( ( { model | errors = List.map (\errorMessage -> (Form, errorMessage)) errorMessages }, Cmd.none), NoOp )
    LoginCompleted (Ok player) ->
      ( (model, Cmd.batch [ storeSession player, Route.modifyUrl Route.Home ]), SetPlayer player )


-- VALIDATION --

type Field
  = Form
  | Username
  | Password

validate : Model -> List Error
validate =
  Validate.all
    [ .username >> ifBlank (Username, "Username can't be blank.")
    , .password >> ifBlank (Password, "Password can't be blank.")
    ]

errorsDecoder : Decoder (List String)
errorsDecoder =
  decode (\username password -> List.concat [ username, password ])
    |> optionalError "username"
    |> optionalError "password"

optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []
