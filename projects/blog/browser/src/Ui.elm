module Ui exposing
  ( container
  , link
  , datePublished
  , h1
  , h2
  , h3
  , spXXS
  , spXS
  , spS
  , spM
  , spL
  , spXL
  , spXXL
  , black
  , white
  , grey
  , green
  , purple
  )

import Element exposing (Element, Color, rgba255)
import Element.Border as Border
import Element.Region as Region
import Element.Font as Font
import Html.Attributes
import Time


link : String -> Element msg -> Element msg
link url child =
  { url = url, label = child }
  |> Element.link
       [ Font.color <| green 1
       , Border.solid
       , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
       , Border.color <| green 0.5
       , Element.mouseOver [Border.color <| green 1]
       ]


container : Element msg -> Element msg
container content =
  Element.el
    [ Element.htmlAttribute <| Html.Attributes.style "width" "100vw"
    , Element.htmlAttribute <| Html.Attributes.style "max-width" ((String.fromInt containerMaxWidth) ++ "px")
    , Element.width Element.fill
    -- Center the container relative to the viewport
    , Element.centerX
    ]
    
    content


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


black : Float -> Color
black = rgba255 58 58 71


white : Float -> Color
white = rgba255 241 239 247


grey : Float -> Color
grey = rgba255 142 150 166


green : Float -> Color
green = rgba255 142 161 165


purple : Float -> Color
purple = rgba255 58 59 95


h1 : List (Element msg) -> Element msg
h1 =
  Element.paragraph
    [ Region.heading 1
    , Font.bold
    , Font.size 32
    , Element.spacingXY 0 4
    ]


h2 : List (Element msg) -> Element msg
h2 =
  Element.paragraph
    [ Region.heading 2
    , Font.bold
    , Font.size 22
    , Element.spacingXY 0 3
    ]


h3 : List (Element msg) -> Element msg
h3 =
  Element.paragraph
    [ Region.heading 3
    , Font.semiBold
    , Font.size 20
    , Element.spacingXY 0 2
    ]


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