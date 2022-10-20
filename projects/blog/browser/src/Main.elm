module Main exposing (..)

import Browser
import Init exposing (init)
import Model exposing (Model, Msg)
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
    , onUrlChange = Model.UrlChanged
    , onUrlRequest = Model.LinkClicked
    }
