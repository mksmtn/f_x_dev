module Components exposing (datePublished)

import Time

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
