module Page.Register exposing (..)

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
  , firstName : String
  , lastName : String
  , blurb : String
  , email : String
  }

type alias Error =
  ( Field, String )

initialModel : Model
initialModel =
  { errors = []
  , username = ""
  , password = ""
  , firstName = ""
  , lastName = ""
  , blurb = ""
  , email = ""
  }

-- VIEW --
view : Session -> Model -> Html Msg
view session model =
  div [ class "auth-page", style [ ( "text-align", "center" ) ]]
    [ Form.viewErrors model.errors
    , viewForm 
    ]

viewForm : Html Msg
viewForm =
  Html.form [ onSubmit SubmitForm ]
    [ inputFor "Username" Username "text"
    , inputFor "Password" Password "password"
    , inputFor "First Name" FirstName "text"
    , inputFor "Last Name" LastName "text"
    , inputFor "Message" Blurb "text"
    , inputFor "Email" Email "text"
    , button [ class "btn blue waves-effect" ]
        [ text "Sign Up" ]
    ]

inputFor : String -> (String -> RegistrationAttr) -> String -> Html Msg
inputFor holder attr inputType =
  input
    [ placeholder holder
    , type_ inputType
    , onInput (\s -> Set (attr s))
    ]
    []

-- UPDATE --
type Msg
  = SubmitForm
  | Set RegistrationAttr
  | RegistrationCompleted (Result Http.Error Player)

type ExternalMsg
  = NoOp
  | SetPlayer Player

type RegistrationAttr
 = Username String
 | Password String
 | FirstName String
 | LastName String
 | Blurb String
 | Email String

type Field
  = Name
  | Pass
  | E_mail
  | Server

validate : Model -> List Error
validate =
  Validate.all
    [ .username >> ifBlank (Name, "Username can't be blank.")
    , .password >> ifBlank (Pass, "Password can't be blank.")
    , .email >> ifBlank (E_mail, "Email can't be blank.")
    ]

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
  case msg of
    SubmitForm ->
      case validate model of
        [] ->
          ( ( model, Http.send RegistrationCompleted (Request.Player.register model) ), NoOp )
        errors ->
          ( ( { model | errors = errors }, Cmd.none ), NoOp)
    Set (Username name) ->
      ( ({ model | username = name }, Cmd.none), NoOp )
    Set (Password password) ->
      ( ({ model | password = password }, Cmd.none), NoOp )
    Set (FirstName firstName) ->
      ( ( { model | firstName = firstName }, Cmd.none), NoOp )
    Set (LastName lastName) ->
      ( ( { model | lastName = lastName }, Cmd.none), NoOp )
    Set (Blurb blurb) ->
      ( ( { model | blurb = blurb }, Cmd.none), NoOp )
    Set (Email email) ->
      ( ( { model | email = email }, Cmd.none), NoOp )
    RegistrationCompleted (Err error) ->
      ( ( { model | errors = [ (Server, toString error) ] }, Cmd.none), NoOp )
    RegistrationCompleted (Ok player) ->
      ( ( model , Cmd.batch [ storeSession player, Route.modifyUrl Route.Home ] ), SetPlayer player )
