module Subscriptions exposing (subscriptions)

import Model exposing (..)
import Browser.Events exposing (onKeyDown)
import Time
import Json.Decode as D

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ keyDownSubscription, timeSubscription model ]

keyDownSubscription : Sub Msg 
keyDownSubscription =
    onKeyDown (D.map (KeyDown) keyDecoder)

timeSubscription : Model -> Sub Msg 
timeSubscription model = 
    if model.status == Running then 
        Time.every clock (\_ -> Step)
    else
        Sub.none