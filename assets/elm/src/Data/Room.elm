module Data.Room exposing (..)

import Data.Player as Player exposing (Player, Username, TablePlayer, usernameDecoder, tablePlayerDecoder)
import Data.Card as Card exposing (Card)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Dict as Dict exposing (Dict)

type alias Room =
  {
    active : Maybe Username
  , currentBigBlind : Maybe Int
  , currentSmallBlind : Maybe Int
  , state : String
  , paid : ChipTracker
  , toCall : Int
  , players : List TablePlayer
  , chipRoll : ChipTracker
  , seating : List Seating
  , playerHands : List PlayerHand
  , round : ChipTracker
  , pot : Int
  , table : Table
  }
  
type alias ChipTracker = Dict String Int

type alias Seating =
  { name : Username
  , position : Int
  }
  
type alias PlayerHand =
  { player : Username
  , hand : List Card
  }
  
type alias Table =
  List Card

decoder : Decoder Room
decoder =
  decode Room
    |> required "active" (Decode.nullable usernameDecoder)
    |> required "current_big_blind" (Decode.nullable Decode.int)
    |> required "current_small_blind" (Decode.nullable Decode.int)
    |> required "state" Decode.string
    |> required "paid" chipTrackerDecoder
    |> required "to_call" Decode.int
    |> required "players" (Decode.list tablePlayerDecoder)
    |> required "chip_roll" chipTrackerDecoder
    |> required "seating" (Decode.list seatingDecoder)
    |> required "player_hands" (Decode.list playerHandDecoder)
    |> required "round" chipTrackerDecoder
    |> required "pot" Decode.int
    |> required "table" (Decode.list Card.decoder)
    
chipTrackerDecoder : Decoder ChipTracker
chipTrackerDecoder =
  Decode.dict Decode.int
  
seatingDecoder : Decoder Seating
seatingDecoder =
  decode Seating
    |> required "name" usernameDecoder
    |> required "position" Decode.int
    
playerHandDecoder : Decoder PlayerHand
playerHandDecoder =
  decode PlayerHand
    |> required "player" usernameDecoder
    |> required "hand" (Decode.list Card.decoder)
    
defaultRoom : Room
defaultRoom =
  { active = Nothing
  , currentBigBlind = Nothing
  , currentSmallBlind = Nothing
  , state = "idle"
  , paid = Dict.empty
  , toCall = 0
  , players = []
  , chipRoll = Dict.empty
  , seating = []
  , playerHands = []
  , round = Dict.empty
  , pot = 0
  , table = []
  }