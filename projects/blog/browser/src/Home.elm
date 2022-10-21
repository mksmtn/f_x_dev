module Home exposing (view)

import Element exposing (Element, rgb, rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Time
import Model exposing (ArticlePreview, Msg, Model)
import RemoteData
import Html.Attributes
import Ui


view : Model -> Element Msg
view model =
  let
    previews =
      case model.latestArticles of
        RemoteData.Success articles ->
          articles
          |> List.map (articlePreview model.zone)

        RemoteData.Failure _ ->
          [Element.text "Произошла ошибка"]
        
        RemoteData.Loading ->
          [Element.text "Загрузка.."]
        
        RemoteData.NotAsked ->
          [Element.text "..."]
  in
  previews
  |> (::) latestHeader
  |> Element.column
       [ Element.spacingXY 0 Ui.spL
       , Element.paddingEach { top = round (model.viewport.viewport.height / 8), bottom = Ui.spM, left = 0, right = 0 }
       ]


latestHeader : Element msg
latestHeader =
  [ Element.text "ПОСЛЕДНИЕ СТАТЬИ" ]
  |> Element.paragraph
       [ Font.bold
       , Element.spacingXY 0 Ui.spS
       ]


cover : Element msg
cover =
  { src = "/assets/images/f_x_dev_logo.min.jpg", description = "f(x) logo in front of the sea" }
  |> Element.image
       [ Element.height <| Element.px 600
       , Element.width Element.fill
       , Element.htmlAttribute <| Html.Attributes.style "filter" "contrast(50%) brightness(90%)"
       ]
  |> Element.el
       [ Border.solid
       , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
       , Border.color <| Ui.black 1
       , Element.width Element.fill
       ]
  |> Element.el
       [ Element.paddingEach { top = 0, left = 0, right = 0, bottom = Ui.spL }
       , Element.width Element.fill
       ]


articlePreview : Time.Zone -> ArticlePreview -> Element Msg
articlePreview zone article =
  [ Element.text <| Ui.datePublished zone article.publishedAt
  , Element.text " · "
  , Element.text <| (String.fromInt article.minsToRead) ++ " мин"
  ]
  |> Element.row [ Font.color <| Ui.grey 1 ]
  |> List.singleton
  |> (::) (Element.link [] { url = "/articles/" ++ article.slug, label = Ui.h3 [ Element.text article.title ] })
  |> Element.column
       [ Element.spacingXY 0 Ui.spXS ]
