module View exposing (view)

import About
import Article
import Consts
import Contacts
import Model exposing (Model, Msg(..))
import Route exposing (Route(..))
import Html exposing (Html)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Region
import RemoteData
import Home
import Ui
import Browser


routerOutlet : Model -> Element Msg
routerOutlet model = 
  let
    page =
      case model.route of
        About ->
          About.view
        
        Article _ ->
          Article.view model.article model.zone

        Contacts ->
          Contacts.view

        Home ->
          Home.view model
        
        NotFound ->
          Element.text "Not found"
  in
  page
  |> Element.el
       [ Element.paddingXY Ui.spS 0
       , Element.width Element.fill
       ]


view : Model -> Browser.Document Msg
view model =
  let
    title =
      case model.route of
          Route.Article _ ->
            case model.article of
              RemoteData.Success article ->
                "f(x) | " ++ article.preview.title
              _ ->
                "f(x) Блог"
          Route.About ->
            "f(x) | О сайте"
          Route.Contacts ->
            "f(x) | Контакты"
          Route.Home ->
            "f(x) | Последние статьи"
          Route.NotFound ->
            "f(x) | 404"
  in
  { title = title
  , body =
    [ body model
    ]
  }


body : Model -> Html Msg
body model =
  [ header
  , Ui.container <| routerOutlet model
  , Element.el [ Element.height Element.fill ] Element.none
  , footer
  ]
  |> Element.column [ Element.width Element.fill, Element.height Element.fill ]
  |> Element.layout
      [ Element.fill
        |> Element.maximum (round model.viewport.viewport.width)
        |> Element.width
      , Element.fill
        |> Element.minimum (round model.viewport.viewport.height)
        |> Element.height
      , Background.color <| Ui.white 1
      , Font.color <| Ui.black 1
      , Element.scrollbarY
      ]


header : Element Msg
header =
  [ headerLogo
  -- , Element.el [ Element.alignRight ] nav
  ]
  |> Element.row
       [ Element.width Element.fill
       , Element.paddingXY Ui.spS Ui.spXS
       ]
  |> Ui.container
  |> Element.el
       [ Element.width Element.fill
       , Background.color <| Ui.purple 1
       , Font.color <| Ui.white 1
       ]


headerLogo : Element Msg
headerLogo =
  { url = Route.toString Route.Home
  , label = Element.text "[f(x)]"
  }
  |> Element.link []


nav : Element Msg
nav =
  [ Element.link [] { url = Route.toString Route.About, label = Element.text "О сайте" }
  , Element.link [] { url = Route.toString Route.Contacts, label = Element.text "Контакты" }
  ]
  |> Element.row
       [ Element.spacingXY Ui.spS 0
       , linkPadding
       , Element.width Element.fill
       , Element.Region.navigation
       ]


linkPadding = Element.paddingEach {right = Ui.spXXS, top = 1, bottom = 1, left = 0}


footerLinks : Element Msg
footerLinks =
  [ { url = Route.toString Route.Home, label = Element.text "Главная" }
  , { url = Route.toString Route.About, label = Element.text "О сайте" }
  , { url = Route.toString Route.Contacts, label = Element.text "Контакты" }
  , { url = Consts.vkGroupLink, label = Element.text "Вконтакте" }
  , { url = Consts.tgChannelLink, label = Element.text "Телеграм" }
  ]
  |> List.map (Element.link [ linkPadding ])
  |> Element.wrappedRow [ Element.spacingXY Ui.spS 0 ]


footerCopyright : Element Msg
footerCopyright =
  Element.text "© 2022-2023 Maksim Matyunin"
  |> Element.el
       [ Element.centerX
       , Element.spacingXY 0 Ui.spS
       ]


footer : Element Msg
footer =
  [footerLinks, footerCopyright]
  |> Element.column
       [ Element.width Element.fill
       , Element.spacingXY 0 Ui.spL
       , Element.paddingXY Ui.spS Ui.spM
       ]
  |> Ui.container
  |> Element.el
     [ Background.color <| Ui.purple 1
     , Font.color <| Ui.white 0.86
     , Element.Region.footer
     , Element.width Element.fill
     ]
