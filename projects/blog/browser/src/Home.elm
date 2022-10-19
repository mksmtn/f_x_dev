module Home exposing (view, ArticlePreview)

import Element exposing (Element)
import Msg exposing (Msg)
import Time


view : Time.Zone -> List ArticlePreview -> Element Msg
view zone articles =
  articles
  |> List.map (articlePreview zone)
  |> (::) (Element.text "Последние статьи")
  |> Element.column []


articlePreview : Time.Zone -> ArticlePreview -> Element Msg
articlePreview zone article =
  [ Element.text <| datePublished zone article.publishedAt, Element.text " ~ ", Element.text <| String.fromInt article.minsToRead ]
  |> Element.row []
  |> List.singleton
  |> (::) (Element.link [] { url = "/articles/" ++ article.slug, label = Element.text article.title })
  |> Element.column []


datePublished : Time.Zone -> Int -> String
datePublished zone timestamp =
  let
      posix = Time.millisToPosix timestamp
      month = Time.toMonth zone posix
      day = Time.toDay zone posix

      monthString =
        case month of
          Time.Jan -> "янв"
          Time.Feb -> "февр"
          Time.Mar -> "март"
          Time.Apr -> "апр"
          Time.May -> "мая"
          Time.Jun -> "июня"
          Time.Jul -> "июля"
          Time.Aug -> "авг"
          Time.Sep -> "сент"
          Time.Oct -> "окт"
          Time.Nov -> "ноября"
          Time.Dec -> "дек"
  in
  [ String.fromInt day, monthString ]
  |> String.join " "


type alias ArticlePreview =
  { title : String
  , publishedAt : Int
  , minsToRead : Int
  , slug : String
  }
