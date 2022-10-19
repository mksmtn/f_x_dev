module Main exposing (..)

import Browser
import Init exposing (init)
import Msg exposing (Msg)
import Model exposing (Model)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = Msg.UrlChanged
    , onUrlRequest = Msg.LinkClicked
    }
