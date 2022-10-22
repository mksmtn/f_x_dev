port module Update exposing (update)

import Commands
import Route
import Model exposing (Model, Msg(..))
import Url
import Url.Parser as Parser
import Browser
import Browser.Navigation as Nav
import RemoteData

port highlightAll : () -> Cmd msg
port trackNavigation : String -> Cmd msg


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


changeRoute : Url.Url -> Model -> Model
changeRoute url model =
  { model
    | url = url
    , route = Maybe.withDefault Route.NotFound (Parser.parse Route.parser url)
  }


onUrlChange : Url.Url -> Model -> (Model, Cmd Msg)
onUrlChange url model =
  let
    nextModel = changeRoute url model

    routeCmd =
      case nextModel.route of
        Route.Article slug ->
          Commands.loadArticle slug
        _ ->
          Cmd.none
  
  in
  (nextModel, Cmd.batch [routeCmd, trackNavigation <| Url.toString url])
