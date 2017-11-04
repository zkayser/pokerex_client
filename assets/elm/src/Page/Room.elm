module Page.Room exposing (..)

import Data.Player as Player exposing (Player)
import Data.Session as Session exposing (Session)
import Data.Room as Room exposing (Room)
import Data.AuthToken as AuthToken
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Widgets.PlayerToolbar as PlayerToolbar
import Widgets.Modal as Modal
import Phoenix
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Push as Push exposing (Push)

-- Boiler Plate

type Msg
  = NewMsg String
  | Joined
  | JoinedChannel
  | JoinRoom Player
  | LeaveRoom Player
  | SocketOpened
  | SocketClosed
  | SocketClosedAbnormally
  | AddPlayerSuccess Value

type ExternalMsg
  = NoOp

type alias Model =
  { room : String
  , roomModel : Room
  , roomType : String
  , players : List Player
  , player : Player
  , channelSubscriptions : List (Channel Msg)
  , modalRendered : Bool
  }

-- SOCKET & CHANNEL CONFIG --

socketUrl : String
socketUrl =
  "ws://phoenix-experiment-zkayser.c9users.io:8080/socket/websocket"

socket : Session -> Socket Msg
socket session =
  let
    params =
      case session.player of
        Just player ->
          let
            token = AuthToken.authTokenToString player.token
          in
          [ ( "guardian_token", token )]
        Nothing -> []
  in
  Socket.init socketUrl
    |> Socket.withParams params
    |> Socket.onOpen (SocketOpened)
    |> Socket.onClose (\_ -> SocketClosed)
    |> Socket.onAbnormalClose (\_ -> SocketClosedAbnormally)

room : Model -> Channel Msg
room model =
  Channel.init ("players:" ++ model.room)
    |> Channel.withPayload ( Encode.object [ ("type", Encode.string "public") ] )
    |> Channel.onJoin (\_ -> JoinedChannel)
    |> Channel.on "add_player_success" (\payload -> AddPlayerSuccess payload)
    |> Channel.withDebug


initialModel : Player -> String -> String -> Model
initialModel player roomTitle roomType =
  { room =  roomTitle -- Should be updated to take dynamic values on load
  , roomModel = Room.defaultRoom player
  , roomType = roomType
  , players = []
  , player = player
  , channelSubscriptions = [ ] -- should be initialized to players:#{room_number}
  , modalRendered = False
  }

-- VIEW --

view : Session -> Model -> Html Msg
view session model =
  div [ class "room-container" ] 
    [ div [ class "table-container" ]
      (viewTableCenter :: viewPlayers session model)
    , PlayerToolbar.view (toolbarConfig model)
    , maybeViewModal model
    ]
  
viewPlayers : Session -> Model -> List (Html Msg)
viewPlayers session model =
  List.map (viewSeat) (List.range 1 8)
  
viewTableCenter : Html Msg
viewTableCenter =
  div [ class "table-center" ]
    [ img [ id "deck", src "http://phoenix-experiment-zkayser.c9users.io:8081/images/card-back.svg.png"] [] ]

viewSeat : Int -> Html Msg
viewSeat number =
  div [ id ("seat-" ++ (toString number)), class "player-seat", style [("text-align", "center")] ]
    [ text (toString number) ] 

joinView : Model -> Html Msg
joinView model =
  div [ class "card-content" ]
    [ span [ class "card-title" ] [ text "Join the Game" ]
    , p [] [ text "Enter the amount of chips you would like to bring to the table."]
    , p [] [ text "You must enter with a minimum of 100 chips."]
    , p [] [ text ("Current Chip Amount: " ++ (toString model.player.chips )) ]
    ]

viewJoinActions : Model -> Html Msg
viewJoinActions model =
  div [ class "card-action" ]
    [ a [ class "btn green", onClick Joined ] [ text "Join" ] ] -- Needs editing later on

maybeViewModal model =
  case model.modalRendered of
    True -> Modal.view (joinModalConfig model)
    False -> text ""

-- WIDGET CONFIGURATIONS --

toolbarConfig : Model -> PlayerToolbar.Config Msg
toolbarConfig model =
  let
    hasJoined =
      List.member model.player model.players
    (txt, msg) =
      if hasJoined then ("Leave", LeaveRoom model.player) else ("Join", JoinRoom model.player)
  in
  { joinLeaveMsg = msg, btnText = txt }

joinModalConfig : Model -> Modal.Config Msg
joinModalConfig model =
  { backgroundColor = "white"
  , contentHtml = [ joinView model, viewJoinActions model ]
  } 

-- UPDATE --

update : Msg -> Model -> ( (Model, Cmd Msg), ExternalMsg )
update msg model =
  case msg of
    NewMsg message ->         ( ( model, Cmd.none), NoOp )
    JoinedChannel ->          ( ( model, Cmd.none), NoOp )
    Joined ->                 handleJoined model
    SocketOpened ->           ( ( model, Cmd.none), NoOp )
    SocketClosed ->           ( ( model, Cmd.none), NoOp )
    SocketClosedAbnormally -> ( ( model, Cmd.none), NoOp )
    JoinRoom player ->        ( ( { model | modalRendered = not model.modalRendered }, Cmd.none), NoOp)
    LeaveRoom player ->       handleLeaveRoom player model
    AddPlayerSuccess room ->
      Debug.log ("Got AddPlayerSuccess message with room: " ++ (toString room))
      ( ( model, Cmd.none), NoOp )

-- UPDATE HELPERS --

handleLeaveRoom : Player -> Model -> ( (Model, Cmd Msg), ExternalMsg )
handleLeaveRoom player model =
  let
    filterBy = Player.usernameToString player.username
    newModel =
      { model | players = List.filter (\player -> Player.usernameToString(player.username) /= filterBy) model.players
              , channelSubscriptions = []
      }
  in
  ( (newModel, Cmd.none), NoOp )

handleJoined : Model -> ( (Model, Cmd Msg), ExternalMsg )
handleJoined model =
  let
    newModel =
      { model | modalRendered = False, channelSubscriptions = (room model) :: model.channelSubscriptions } 
  in
  Debug.log ">>>>> HANDLE JOINED CALLED <<<<<"
  ( ( newModel, (addPlayer model)), NoOp )

-- PUSH MESSAGES --
-- "add_player"
addPlayer : Model -> Cmd Msg
addPlayer model =
  let
    payload =
      Encode.object 
        [ ("player", Encode.string <| Player.usernameToString model.player.username )
        , ("room", Encode.string model.room)
        , ("amount", Encode.int 101) -- harcoded for now
        ]
    push =
      Push.init ("players:" ++ model.room) "add_player"
        |> Push.withPayload payload
  in
  Phoenix.push socketUrl push    
  
-- SUBSCRIPTIONS --    

subscriptions : Model -> Session -> Sub Msg
subscriptions model session =
  Phoenix.connect (socket session) model.channelSubscriptions