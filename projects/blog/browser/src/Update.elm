module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Url
import Browser
import Browser.Navigation as Nav


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        
        Browser.External href ->
          ( model, Nav.load href )
    
    UrlChanged url ->
      Model.onUrlChange url model
    
    AdjustTimeZone zone ->
      ({ model | zone = zone }, Cmd.none)
    
    AdjustViewport viewport ->
      ({ model | viewport = viewport }, Cmd.none)
    
    OnResize w h ->
      let
        currentViewport = model.viewport
        nextViewport =
          { currentViewport
          | viewport =
              { width = toFloat w
              , height = toFloat h
              , x = currentViewport.viewport.x
              , y = currentViewport.viewport.y
              }
          }
      in
      ({ model | viewport = nextViewport }, Cmd.none)
