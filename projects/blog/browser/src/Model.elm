module Model exposing (Model, Msg(..), Article, ArticlePreview)

import Browser
import Browser.Navigation as Nav
import Route exposing (Route(..))
import Time
import Url
import Browser.Dom
import RemoteData
import Http


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AdjustTimeZone Time.Zone
  | AdjustViewport Browser.Dom.Viewport
  | OnResize Int Int
  | GotArticle (Result Http.Error Article)
  | GotArticlePreviewList (Result Http.Error (List ArticlePreview))


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , route : Route
  , zone : Time.Zone
  , viewport : Browser.Dom.Viewport
  , article : RemoteData.WebData Article
  , latestArticles : RemoteData.WebData (List ArticlePreview)
  }


type alias Article =
  { preview : ArticlePreview
  , content : String
  }


type alias ArticlePreview =
  { title : String
  , publishedAt : Int
  , minsToRead : Int
  , slug : String
  }
