module Route exposing (Route(..), isAboutRoute, isArticleRoute, isContactsRoute, isHomeRoute, parser, toString)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, top)

type Route
  = About
  | Article String
  | Contacts
  | Home
  | NotFound


toString : Route -> String
toString route =
  case route of
    About ->
      "/about"
    Article slug ->
      "/articles/" ++ slug
    Contacts ->
      "/contacts"
    Home ->
      "/"
    NotFound ->
      "/*"


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map About (s "about")
    , Parser.map Article (s "articles" </> string)
    , Parser.map Contacts (s "contacts")
    , Parser.map Home top
    ]


isAboutRoute : Route -> Bool
isAboutRoute route =
  case route of
    About ->
      True
    _ ->
      False


isArticleRoute : Route -> Bool
isArticleRoute route =
  case route of
    Article _ ->
      True
    _ ->
      False


isHomeRoute : Route -> Bool
isHomeRoute route =
  case route of
    Home ->
      True
    _ ->
      False


isContactsRoute : Route -> Bool
isContactsRoute route =
  case route of
    Contacts ->
      True
    _ ->
      False
    