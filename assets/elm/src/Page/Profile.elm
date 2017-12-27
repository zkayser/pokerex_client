module Page.Profile exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile)
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (class, placeholder, classList, style)
import Html.Events exposing (onClick, onSubmit)

type alias Model =
  { player : Player
  , profile : Profile
  , activeAttribute : UpdatableAttribute
  }

type Msg
  = UpdateEmail String
  | UpdateChips
  | HeaderClicked UpdatableAttribute

type ExternalMsg
  = NoOp

type UpdatableAttribute
  = Email
  | Chips
  | Nothing

-- INITIALIZATION
initialModel : Player -> Model
initialModel player =
  { player = player
  , profile = profileFor player
  , activeAttribute = Nothing
  }

profileFor : Player -> Profile
profileFor player =
  { errors = []
  , username = Player.usernameToString player.username
  , email = player.email
  , chips = player.chips
  , isNewProfile = False
  }

-- VIEW
view : Session -> Model -> Html Msg
view session model =
  div [ class "profile-container"]
    [ h1 [ class "teal-text profile-greeting" ]
      [ text <| (playerGreeting model.player) ]
    , div [ class "profile-pane-container"]
      [ div [ class "profile-pane" ]
       [ ul [ class "collapsible popout"]
        (viewProfileForm model)
       ]
      , div [ class "profile-pane" ] []
      ]
    ]

viewProfileForm : Model -> List (Html Msg)
viewProfileForm model =
  [ li [ ]
    [ div [ class "collapsible-header" ]
      [ text "Username: "
      , text model.profile.username
      ]
    ]
  , li [ classList [ ("active", model.activeAttribute == Email) ] ]
    [ div
      [ classList
        [ ("collapsible-header", True)
        , ("active", model.activeAttribute == Email)
        ]
        , onClick (HeaderClicked Email)
      ]
      [ text <| "Email: " ++ model.profile.email ]
    , div [ class "collapsible-body", styleBodyFor model Email ]
      [ form [ onSubmit (UpdateEmail model.profile.email) ]
        [ div [ class "input-field" ]
          [ input [ placeholder model.player.email ] [] ]
        ]
      ]
    ]
  , li [ classList [ ("active", model.activeAttribute == Chips)]]
    [ div
      [ classList
        [ ( "collapsible-header", True)
        , ( "active", model.activeAttribute == Chips )
        ]
      ,  onClick (HeaderClicked Chips)
      ]
      [ text <| "Chips: " ++ (toString model.profile.chips) ]
    , div [ class "collapsible-body chip-restore", styleBodyFor model Chips ]
      [ form [ onSubmit UpdateChips ]
        [ button [ class "btn blue white-text", onClick UpdateChips ]
          [ text "Restore chip count to 1000" ]
        ]
      ]
    ]
  ]

-- Update
update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    UpdateEmail email ->        handleUpdateEmail model email
    UpdateChips ->              handleUpdateChips model
    HeaderClicked attribute ->  handleHeaderClicked model attribute

handleUpdateEmail : Model -> String -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateEmail model email =
  let
    profile =
      model.profile
    newProfile =
      { profile | email = email }
  in
  ( ( { model | profile = profile }, Cmd.none), NoOp )

handleUpdateChips : Model -> ( ( Model, Cmd Msg), ExternalMsg )
handleUpdateChips model =
  ( ( model, Cmd.none), NoOp )

handleHeaderClicked : Model -> UpdatableAttribute -> ( ( Model, Cmd Msg), ExternalMsg )
handleHeaderClicked model attribute =
  let
    activeAttribute =
      if model.activeAttribute == attribute then Nothing else attribute
    -- The `Chips` field should not be editable unless the player has 100 chips or fewer
    newActiveAttribute =
      if activeAttribute == Chips && model.player.chips > 100 then Nothing else activeAttribute
  in
  ( ( { model | activeAttribute = newActiveAttribute }, Cmd.none), NoOp )

-- SUBSCRIPTIONS
subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Sub.none

-- Helpers
playerGreeting : Player -> String
playerGreeting player =
  let
    name =
      Player.usernameToString player.username
  in
  name ++ "'s Profile"

styleBodyFor : Model -> UpdatableAttribute -> Html.Attribute Msg
styleBodyFor model attribute =
  let
    displayStyle =
      if model.activeAttribute == attribute then "block" else "none"
  in
  style [ ("display", displayStyle) ]