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
import Route


-- Model --

type alias Model =
  { errors : List Error
  , username : String
  , password : String
  }

type alias Error =
  ( String, String )

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
    [ viewForm ]

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
    , button [ style [ ("background-color", "blue"), ("color", "white") ] ]
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
      ( (model, Http.send LoginCompleted (Request.Player.login model) ), NoOp )
    SetUsername name ->
      ( ( { model | username = name }, Cmd.none), NoOp )
    SetPassword pass ->
      ( ( { model | password = pass }, Cmd.none), NoOp )
    LoginCompleted (Err error) ->
      ( ( { model | errors =  [("error", toString error)]}, Cmd.none), NoOp )
    LoginCompleted (Ok player) ->
      ( (model, Cmd.batch [ storeSession player, Route.modifyUrl Route.Home ]), SetPlayer player )
