module Data.Card exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Html exposing (Html, img)
import Html.Attributes exposing (src, id)


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
    
suitToString : Card -> String
suitToString card =
  case card.suit of
    Hearts -> "hearts"
    Diamonds -> "diamonds"
    Spades -> "spades"
    Clubs -> "clubs"
    _ -> "error"
    
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
    
rankToString : Card -> String
rankToString card =
  case card.rank of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "jack"
    Queen -> "queen"
    King -> "king"
    Ace -> "ace"
    _ -> "error"
    
rootCardsAssetsUrl : String
rootCardsAssetsUrl =
  "http://phoenix-experiment-zkayser.c9users.io:8081/images/cards/"
  
cardImageFor : Int -> Card -> Html msg
cardImageFor index card =
  let sourceUrl =
    case (card.rank, card.suit) of
      (RankError, SuitError) -> "http://phoenix-experiment-zkayser.c9users.io:8081/images/card-back.svg.png"
      _ -> (rootCardsAssetsUrl ++ (rankToString card) ++ "_of_" ++ (suitToString card) ++ ".svg")
  in
  img [ src sourceUrl, id ("player-hand-card-" ++ (toString index)) ] []
    
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