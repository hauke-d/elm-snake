
import Browser
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)
import Model exposing (init)


main = 
    Browser.element 
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions 
        }