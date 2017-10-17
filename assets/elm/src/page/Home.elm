module Page.Home exposing (..)

import Data.Session as Session exposing (Session)
import Data.Player as Player exposing (Player, usernameToString)
import Html exposing (..)
import Html.Attributes exposing (style, class)

-- MODEL --

type alias Model =
  { greeting : String }

initialModel : Model
initialModel =
  { greeting = "Welcome to PokerEx"}

-- VIEW --

view : Session -> Model -> Html msg
view session model =
  let
    personalizedGreeting =
      case session.player of
        Just player -> ", " ++ (usernameToString <| player.username)
        Nothing -> "!"
  in      
  main_ []
    [ div [ class "hero valign-wrapper" ]
        [ h1 [ class "welcome", style [ ("text-align", "center")] ] 
          [ text (model.greeting ++ personalizedGreeting) ] 
        ] 
    , div [ class "row landing"]
      [  div [ class "col s12 m4 l4 landing-item" ]
        [ div [ class "card teal darken-2 landing-item" ]
          [ div [ class "card-content white-text" ]
            [ span [ class "card-title" ]
              [ text "Create your own poker room and invite friends"]
            , p [] 
              [ text """Sign up to create your own private poker rooms. Invite friends
                      or send invitations to other PokerEx players. Once you make a
                      private poker room, your game and table state will be kept alive
                      until you decide to close the room."""
              ]
            ]
          ]
        ]
      , div [ class "col s12 m4 l4 landing-item" ]
        [ div [ class "card cyan darken-3 landing-item" ]
          [ div [ class "card-content white-text"]
            [ span [ class "card-title"]
              [ text "What is PokerEx?" ]
            , p [] 
              [ text """PokerEx is an online poker environment
                      designed to deliver an engaging, real-time
                      experience."""
              ]
            ]
          ]
        ]
      , div [ class "col s12 m4 l4 landing-item" ]
        [ div [ class "card indigo darken-4 landing-item" ]
          [ div [ class "card-content white-text" ]
            [ span [ class "card-title" ]
              [ text "Play with other PokerEx players in our public rooms"]
            , p []
              [ text """Once you join us on PokerEx, you can sit down and play at
                      any one of our public rooms. Public rooms are a perfect fit
                      if you are looking to play a short round or two and get on
                      your way."""
              ]
            ]
          ]
        ]
      ]
    ]

-- UPDATE --
type Msg
  = DoNothing -- Remove when actually implementing

type ExternalMsg
  = NoOp

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
  case msg of
    _ ->
      ( ( model, Cmd.none ), NoOp )
