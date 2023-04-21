module Contacts (view) where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A


view :: (String, H.Html)
view =
  ("f(x) | Контакты", contactsView)


contactsView :: H.Html
contactsView = 
  H.section ! A.class_ "container mt-3" $ do
    H.div ! A.class_ "d-flex" $ do
      H.div $ do
        H.p $ do
          "Автор: Максим Матюнин"
        
        H.p $ do
          H.a ! A.href "mailto:krabovm@gmail.com" $ do
            "krabovm@gmail.com"

      H.div ! A.class_ "ms-3" $ do
        H.img ! A.src "/assets/images/mksmtn.jpg" ! A.alt "Фото" ! A.width "80"
