module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Browser.Events

subscriptions : Model -> Sub Msg
subscriptions _ =
  Browser.Events.onResize OnResize
