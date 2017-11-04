module Data.Card exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)


type alias Card = 
  {
    rank : Rank
  , suit : Suit
  }
  
type Suit
  = Hearts
  | Spades
  | Clubs
  | Diamonds
  | SuitError
  
type Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  | RankError
  
stringToSuit : String -> Suit
stringToSuit stringSuit = 
  case stringSuit of
    "hearts" -> Hearts
    "diamonds" -> Diamonds
    "spades" -> Spades
    "clubs" -> Clubs
    _ -> SuitError
    
stringToRank : String -> Rank
stringToRank stringRank =
  case stringRank of
    "two" -> Two
    "three" -> Three
    "four" -> Four
    "five" -> Five
    "six" -> Six
    "seven" -> Seven
    "eight" -> Eight
    "nine" -> Nine
    "ten" -> Ten
    "jack" -> Jack
    "queen" -> Queen
    "king" -> King
    "ace" -> Ace
    _ -> RankError
    
rankDecoder : Decoder Rank
rankDecoder =
  Decode.string 
    |> Decode.andThen (\str -> Decode.succeed (stringToRank str))
  
suitDecoder : Decoder Suit
suitDecoder =
  Decode.string
    |> Decode.andThen (\str -> Decode.succeed (stringToSuit str))

decoder : Decoder Card
decoder =
  decode Card
    |> required "rank" rankDecoder
    |> required "suit" suitDecoder