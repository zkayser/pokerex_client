module Views.Helpers exposing (ActivePage(..))

{-
   Determines which navbar link will be rendered as active
-}


type ActivePage
    = Other
    | Login
    | Registration
    | Home
    | Room
    | Rooms
    | Profile
