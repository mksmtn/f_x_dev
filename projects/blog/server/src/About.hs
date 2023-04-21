module About (view) where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A


view :: (String, H.Html)
view =
  ("f(x) | О сайте", aboutView)


aboutView :: H.Html
aboutView =
  H.section ! A.class_ "container mt-3" $ do
    H.p $ do
      H.span "Меня зовут Максим Матюнин, я занимаюсь веб программированием около пяти лет. "
      H.span "Меня интересуют функциональные языки программирования, такие как Haskell и Elm, и применение их идей в ежедневной работе. "
      H.span "Данный сайт, названный "
      H.em "f(x)"
      H.span " («эф от икс»), будет местом, куда я буду выкладывать статьи о функциональных подходах к веб-разработке."
