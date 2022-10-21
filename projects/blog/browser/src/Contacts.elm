module Contacts exposing (view)

import Consts
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Ui


view : Element msg
view =
  [ Element.paragraph
      [ Element.paddingEach { bottom = Ui.spS, top = 0, left = 0, right = 0 } ]
      [ Element.text "Подписывайся в соц. сетях, чтобы не пропустить последние статьи:" ]
  
  , Element.column
      [ Element.width Element.fill
      , Element.paddingEach { bottom = Ui.spM, top = 0, left = 0, right = 0 }
      , Element.spacingXY 0 Ui.spXXS
      ]
      [ Element.el [ Element.width Element.fill ] <| Ui.link Consts.vkGroupLink (Element.text "Группа Вконтакте")
      , Element.el [ Element.width Element.fill ] <| Ui.link Consts.tgChannelLink (Element.text "Канал в Телеграме")
      ]
  
  , Element.row
      [ Element.width Element.fill ]
      [ Element.image
          [ Element.width Element.fill
          , Border.rounded 100
          , Border.solid
          , Border.color <| Ui.black 1
          , Element.clip
          ]
          { src = "/assets/images/mksmtn.jpg", description = "Фото" }
        |> Element.el
           [ Element.width <| Element.px Ui.spXL
           , Element.padding Ui.spXS
           ]
      , Element.column
          [ Element.spacingXY 0 Ui.spXXS
          , Element.width Element.fill
          ]
          [ Element.text "Автор: Максим Матюнин"
          , Element.paragraph
              []
              [ Element.text "Email: "
              , Ui.link "mailto:krabovm@gmail.com" <| Element.text "krabovm@gmail.com"
              ]
          ]
      ]
  ]
  |> Element.column
     [ Element.paddingXY 0 Ui.spM
     , Element.centerY
     , Element.width Element.fill
     ]
