module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Browser.Events

subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onResize OnResize
