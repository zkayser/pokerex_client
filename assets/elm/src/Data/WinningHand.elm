module Data.WinningHand exposing (..)

import Data.Card as Card exposing (Card)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias WinningHand =
    { cards : List Card
    , winner : String
    , handType : String
    }


decoder : Decoder WinningHand
decoder =
    decode WinningHand
        |> required "cards" (Decode.list Card.decoder)
        |> required "winner" Decode.string
        |> required "type" Decode.string
