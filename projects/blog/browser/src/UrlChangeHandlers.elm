module UrlChangeHandlers exposing (changeRoute, onUrlChange)

import Commands
import Url.Parser as Parser
import Route
import Model exposing (Model, Msg)
import Url

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
  (nextModel, routeCmd)
