module Msg exposing (Msg(..))

import Browser
import Time
import Url
import Browser.Dom

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AdjustTimeZone Time.Zone
  | AdjustViewport Browser.Dom.Viewport
  | OnResize Int Int
