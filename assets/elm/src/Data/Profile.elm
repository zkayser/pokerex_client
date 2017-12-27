module Data.Profile exposing (..)

type alias Profile =
  { errors : List Error
  , username : String
  , email : String
  , chips : Int
  , isNewProfile : Bool
  }

type alias Error =
  ( Field, String )

type Field
  = Chips
  | E_mail
  | Server

initialProfile : Profile
initialProfile =
  { errors = []
  , username = ""
  , email = ""
  , chips = 1000
  , isNewProfile = True
  }