module Model exposing (Model, changeRoute, onUrlChange)

import Browser.Navigation as Nav
import Route exposing (Route(..), parser)
import Msg exposing (Msg(..))
import Home exposing (ArticlePreview)
import Time
import Url
import Url.Parser as Parser
import Browser.Dom

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , route : Route
  , zone : Time.Zone
  , viewport : Browser.Dom.Viewport
  , latestArticles : List ArticlePreview
  }

changeRoute : Url.Url -> Model -> Model
changeRoute url model =
  { model
    | url = url
    , route = Maybe.withDefault NotFound (Parser.parse parser url)
  }

onUrlChange : Url.Url -> Model -> (Model, Cmd Msg)
onUrlChange url model =
  let
    nextModel = changeRoute url model
  in
  (nextModel, Cmd.none)
