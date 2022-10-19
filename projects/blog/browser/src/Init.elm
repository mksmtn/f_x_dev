module Init exposing (init)

import Url
import Model exposing (Model)
import Route exposing (Route(..))
import Home exposing (ArticlePreview)
import Browser.Navigation as Nav
import Time
import Task
import Msg exposing (Msg(..))
import Browser.Dom


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let
    (routedModel, routingCmd) = Model.onUrlChange url
      { key = key
      , url = url
      , route = NotFound
      , zone = Time.utc
      , viewport = { scene = { width = 0, height = 0 }, viewport = { x = 0, y = 0, width = 0, height = 0 } }
      , latestArticles = latestArticles
      }
  in
  ( routedModel
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform AdjustViewport Browser.Dom.getViewport
      , routingCmd
      ]
  )


latestArticles : List ArticlePreview
latestArticles =
  [ { title = "Обзор паттерн матчинга в Python"
    , slug = "pattern-matching-in-python-01"
    , minsToRead = 5
    , publishedAt = 1666201951518
    }
  , { title = "Маппинг типов в Typescript на примере"
    , slug = "typescript-types-mapping-example-01"
    , minsToRead = 3
    , publishedAt = 1666201951518
    }
  ]