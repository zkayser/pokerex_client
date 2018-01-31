module Util exposing ((=>), appendErrors, onClickStopPropagation, pair, viewIf)

import Html exposing (Attribute, Html)
import Html.Events exposing (Options, defaultOptions, onWithOptions)
import Json.Decode as Decode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>


pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click" noBubble (Decode.succeed msg)


noBubble : Options
noBubble =
    { stopPropagation = True
    , preventDefault = True
    }


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }
