port module Update exposing (update)

import Model exposing (Model, Msg(..))
import Url
import Browser
import Browser.Navigation as Nav
import UrlChangeHandlers exposing (onUrlChange)
import RemoteData

port highlightAll : () -> Cmd msg


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
      onUrlChange url model
    
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

    GotArticle articleResult ->
      case articleResult of
        Ok article ->
          ({ model | article = RemoteData.Success article }, highlightAll ())
        
        Err err ->
          ({ model | article = RemoteData.Failure err }, Cmd.none)
    
    GotArticlePreviewList listResult ->
      case listResult of
        Ok list ->
          ({ model | latestArticles = RemoteData.Success list }, Cmd.none)
        
        Err err ->
          ({ model | latestArticles = RemoteData.Failure err }, Cmd.none)
