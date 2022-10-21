module About exposing (view)

import Element exposing (Element)
import Ui


view : Element msg
view =
  [ Element.paragraph
      []
      [ Element.text "«Эф от икс». Статьи о функциональных подходах к веб-разработке." ]
  
  , Element.paragraph
      []
      [ Ui.link "/contacts" <| Element.text "Пиши"
      , Element.text ", если хочешь помогать развивать проект!"
      ]
  ]
  
  |> Element.textColumn
       [ Element.paddingXY 0 Ui.spM
       , Element.width Element.fill
       ]
