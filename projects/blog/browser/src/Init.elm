module Init exposing (init)

import Url
import Model exposing (Model, Msg)
import Route exposing (Route(..))
import Browser.Navigation as Nav
import Time
import Task
import Browser.Dom
import RemoteData
import Update exposing (onUrlChange)
import Commands exposing (loadArticlePreviewList)


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let
    (routedModel, routingCmd) = onUrlChange url
      { key = key
      , url = url
      , route = NotFound
      , zone = Time.utc
      , viewport = { scene = { width = 0, height = 0 }, viewport = { x = 0, y = 0, width = 0, height = 0 } }
      , article = RemoteData.NotAsked
      , latestArticles = RemoteData.NotAsked
      }
  in
  ( routedModel
  , Cmd.batch
      [ Task.perform Model.AdjustTimeZone Time.here
      , Task.perform Model.AdjustViewport Browser.Dom.getViewport
      , routingCmd
      , loadArticlePreviewList
      ]
  )
