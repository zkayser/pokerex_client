module Data.RoomPage exposing (RoomPage)

import Data.Room as Room exposing (Room)
import Data.Player as Player exposing (Player)
import Types.Room.ModalState as ModalState exposing (ModalState)
import Types.Room.Messages as Messages exposing (RoomMsg)
import Views.Chat as Chat exposing (Chat)
import Phoenix.Channel as Channel exposing (Channel)

type alias RoomPage =
  { room : String
  , roomModel : Room
  , roomType : String
  , roomMessages : List String
  , players : List Player
  , player : Player
  , joinValue : String
  , joined : Bool
  , channelSubscriptions : List (Channel RoomMsg)
  , modalRendered : ModalState
  , errorMessages : List String
  , raiseAmount : Int
  , raiseInterval : Int
  , chipsAvailable : Int
  , chipsAvailableForJoin : Int
  , addAmount : Int
  , chat : Chat
  , currentChatMsg : String
  }