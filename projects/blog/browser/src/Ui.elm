module Ui exposing (container, spXXS, spXS, spS, spM, spL, spXL, spXXL)

import Element exposing (Element)

container : Element msg -> Element msg
container content =
  Element.row [ Element.centerX, Element.width (Element.maximum containerMaxWidth Element.fill) ] [ content ]


containerMaxWidth : Int
containerMaxWidth = 1192


spXXS : Int
spXXS = 5


spXS : Int
spXS = 10


spS : Int
spS = 15


spM : Int
spM = 25


spL : Int
spL = 40


spXL : Int
spXL = 60


spXXL : Int
spXXL = 80