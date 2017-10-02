module Data.Session exposing (Session, attempt)

import Data.AuthToken exposing (AuthToken)
import Data.Player as Player exposing (Player)

type alias Session =
  { player : Maybe Player }

attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
  case Maybe.map .token session.player of
    Nothing ->
      ( [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "."], Cmd.none )

    Just token ->
      ( [], toCmd token )
