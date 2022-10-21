module Article exposing (view)

import Ui
import Element exposing (Element)
import Element.Font as Font
import Model exposing (Model, Article, Msg)
import RemoteData
import Model exposing (ArticlePreview)
import Markdown.Parser as Md
import Markdown.Renderer
import MarkdownRenderer
import Html
import Time


view : RemoteData.WebData Article -> Time.Zone -> Element Msg
view articleRd zone =
  case articleRd of
    RemoteData.NotAsked -> 
      Element.text "..."
    
    RemoteData.Loading ->
      Element.text "Loading.."
    
    RemoteData.Failure _ ->
      [ Element.text "Произошла ошибка" ]
      |> Element.column []
    
    RemoteData.Success article ->
      [ meta zone article.preview
      , content article.content
      ]
      |> Element.column
           [ Element.width Element.fill
           , Element.paddingXY 0 Ui.spM
           , Element.spacingXY 0 Ui.spS
           ]


content : String -> Element Msg
content markdown =
  markdown
  |> Md.parse
  |> Result.mapError deadEndsToString
  |> Result.andThen (Markdown.Renderer.render MarkdownRenderer.renderer)
  |> Result.map (Element.textColumn [ Element.width Element.fill, Element.spacingXY 0 Ui.spM ])
  |> Result.withDefault (Element.text "Ошибка парсинга статьи")


meta : Time.Zone -> ArticlePreview -> Element Msg
meta zone article =
  [ Element.text <| Ui.datePublished zone article.publishedAt
  , Element.text " · "
  , Element.text ((String.fromInt article.minsToRead) ++ "мин")
  -- , Element.text " | "
  -- , Element.el [ Font.color <| Ui.black 1 ] <| Element.text "Поделиться в вк"
  ]
  |> Element.row
     [ Element.width Element.fill
     , Font.color <| Ui.grey 1
     ]


deadEndsToString deadEnds =
    deadEnds
        |> List.map Md.deadEndToString
        |> String.join "\n"