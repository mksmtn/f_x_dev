module Home exposing (view)

import Element exposing (Element)
import Time
import Model exposing (ArticlePreview, Msg, Model)
import RemoteData
import Components


view : Model -> Element Msg
view model =
  case model.latestArticles of
    RemoteData.Success articles ->
      articles
      |> List.map (articlePreview model.zone)
      |> (::) (Element.text "Последние статьи")
      |> Element.column []
    
    RemoteData.Failure _ ->
      Element.text "Произошла ошибка"
    
    RemoteData.Loading ->
      Element.text "Загрузка.."
    
    RemoteData.NotAsked ->
      Element.text "..."



articlePreview : Time.Zone -> ArticlePreview -> Element Msg
articlePreview zone article =
  [ Element.text <| Components.datePublished zone article.publishedAt, Element.text " ~ ", Element.text <| (String.fromInt article.minsToRead) ++ " мин" ]
  |> Element.row []
  |> List.singleton
  |> (::) (Element.link [] { url = "/articles/" ++ article.slug, label = Element.text article.title })
  |> Element.column []

