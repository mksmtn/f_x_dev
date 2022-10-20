module Decoders exposing (articleDecoder, articlePreviewListDecoder)

import Json.Decode as Decode exposing (Decoder, string, int)
import Model exposing (Article, ArticlePreview)


articleDecoder : Decoder Article
articleDecoder =
  Decode.map2 Article
    (Decode.field "preview" articlePreviewDecoder)
    (Decode.field "content" string)
  |> Decode.field "data"


articlePreviewDecoder : Decoder ArticlePreview
articlePreviewDecoder =
  Decode.map4 ArticlePreview
    (Decode.field "title" string)
    (Decode.field "publishedAt" int)
    (Decode.field "minsToRead" int)
    (Decode.field "slug" string)


articlePreviewListDecoder : Decoder (List ArticlePreview)
articlePreviewListDecoder =
  Decode.list articlePreviewDecoder
  |> Decode.field "data"
