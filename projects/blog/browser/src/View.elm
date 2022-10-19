module View exposing (view)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Html exposing (Html)
import Element exposing (Element)
import Home
import Ui
import Browser
import Element.Region


routerOutlet : Model -> Element Msg
routerOutlet model = 
  case model.route of
    About ->
      Element.text "About"
    
    Article slug ->
      Element.text slug

    Contacts ->
      Element.text "Contact me"

    Home ->
      Home.view model.zone model.latestArticles
    
    NotFound ->
      Element.text "Not found"


view : Model -> Browser.Document Msg
view model =
  { title = "f(x) blog"
  , body =
    [ body model
    ]
  }


body : Model -> Html Msg
body model =
  [ Ui.container header
  , Ui.container <| routerOutlet model
  , Element.el [ Element.height Element.fill ] Element.none
  , Ui.container <| footer
  ]
  |> Element.column [ Element.width Element.fill, Element.height Element.fill ]
  |> Element.layout
      [ Element.width Element.fill
      , Element.fill
        |> Element.minimum (round model.viewport.viewport.height)
        |> Element.height
      ]


header : Element Msg
header =
  [ headerLogo, Element.el [ Element.alignRight ] nav ]
  |> Element.row [ Element.width Element.fill ]


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
       [ Element.spacingXY Ui.spXS 0
       , Element.paddingEach {left = Ui.spXS, right = 0, top = 1, bottom = 1}
       , Element.width Element.fill
       , Element.Region.navigation
       ]

footerLinks : Element Msg
footerLinks =
  [ { url = Route.toString Route.About, label = Element.text "О сайте" }
  , { url = Route.toString Route.Contacts, label = Element.text "Контакты" }
  , { url = "https://vk.com/f_x_dev", label = Element.text "Вконтакте" }
  , { url = "https://t.me/f_x_dev", label = Element.text "Телеграм" }
  ]
  |> List.map (Element.link [Element.paddingEach {right = Ui.spXXS, top = 1, bottom = 1, left = 0}])
  |> Element.wrappedRow [Element.spacingXY Ui.spS 0]


footerCopyright : Element Msg
footerCopyright =
  Element.text "© 2022-2023 Maksim Matyunin"
  |> Element.el [ Element.centerX ]


footer : Element Msg
footer =
  [footerLinks, footerCopyright]
  |> Element.column [ Element.width Element.fill, Element.Region.footer ]
