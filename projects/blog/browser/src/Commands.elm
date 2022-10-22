module Commands exposing (loadArticle, loadArticlePreviewList)

import Http
import Decoders exposing (articleDecoder)
import Decoders exposing (articlePreviewListDecoder)
import Model exposing (Msg)


apiBaseUrl : String
apiBaseUrl = "/api"

loadArticle : String -> Cmd Msg
loadArticle slug =
  Http.get
    { url = apiBaseUrl ++ "/articles/" ++ slug
    , expect = Http.expectJson Model.GotArticle articleDecoder
    }


loadArticlePreviewList :  Cmd Msg
loadArticlePreviewList =
  Http.get
    { url = apiBaseUrl ++ "/articles"
    , expect = Http.expectJson Model.GotArticlePreviewList articlePreviewListDecoder
    }
