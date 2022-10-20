module Article exposing (view)

import Components
import Json.Encode as E
import Element exposing (Element)
import Model exposing (Model, Msg)
import RemoteData exposing (RemoteData(..))
import Model exposing (ArticlePreview)
import Markdown.Parser as Md
import Markdown.Renderer as MdRenderer
import Html
import Time
import Html.Attributes


view : Model -> Element Msg
view model =
  case model.article of
    RemoteData.NotAsked -> 
      Element.text "..."
    
    RemoteData.Loading ->
      Element.text "Loading.."
    
    RemoteData.Failure _ ->
      [ Element.text "Произошла ошибка" ]
      |> Element.column []
    
    RemoteData.Success article ->
      [ meta model.zone article.preview
      , Element.text article.preview.title
      , content article.content
      ]
      |> Element.column []


content : String -> Element Msg
content markdown =
  case
    markdown
    |> Md.parse
    |> Result.mapError deadEndsToString
    |> Result.andThen (\ast -> MdRenderer.render MdRenderer.defaultHtmlRenderer ast)
  of
    Ok rendered ->
      rendered
      |> Html.article []
      |> Element.html
    Err _ ->
      Element.text "Ошибка парсинга статьи"


meta : Time.Zone -> ArticlePreview -> Element Msg
meta zone article =
  [ Element.text <| Components.datePublished zone article.publishedAt
  , Element.text " ~ "
  , Element.text ((String.fromInt article.minsToRead) ++ "мин")
  , Element.text " | "
  , Element.text "Поделиться в вк"
  ]
  |> Element.row []



deadEndsToString deadEnds =
    deadEnds
        |> List.map Md.deadEndToString
        |> String.join "\n"