module MarkdownRenderer exposing (renderer)

import Ui
import Markdown.Renderer
import Markdown.Block as Block
import Markdown.Html
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html
import Html.Attributes

renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ destination } body ->
            Ui.link destination (Element.paragraph [] body)
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            { src = image.src, description = image.alt }
            |> Element.image
                 [ Element.width Element.fill
                 , Element.paddingEach { bottom = Ui.spXS, top = 0, left = 0, right = 0 }
                 ]
    , blockQuote =
        \children ->
            Element.paragraph
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding Ui.spXS
                , Border.color <| Ui.green 1 
                , Background.color <| Ui.green 0.12
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Block.ListItem task children) ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph
                                    [ Element.alignTop ]
                                    ((case task of
                                        Block.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Block.CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        Block.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ ". ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html = Markdown.Html.oneOf []
    }


alternateTableRowBackground =
    Element.rgb255 245 247 249


tableBorder =
    [ Border.color (Element.rgb255 223 226 229)
    , Border.width 1
    , Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, children } =
    case level of
        Block.H1 ->
            Ui.h1 children
            |> Element.el [ Element.paddingEach { bottom = Ui.spS, top = 0, left = 0, right = 0 } ]

        Block.H2 ->
            Ui.h2 children
            |> Element.el [ Element.paddingEach { bottom = Ui.spXS, top = Ui.spS, left = 0, right = 0 } ]

        _ ->
            Ui.h3 children
            |> Element.el [ Element.paddingEach { bottom = Ui.spXXS, top = Ui.spXS, left = 0, right = 0 } ]


code : String -> Element msg
code snippet =
    Element.text snippet
    |> Element.el
         [ Background.color <| Ui.green 0.17
         , Border.rounded 2
         , Element.paddingXY 5 3
         ]


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    let
      language = Maybe.withDefault "plain" details.language
    in
    Html.code
        [ Html.Attributes.class ("language-" ++ language) ]
        [ Html.text details.body ]
    |> List.singleton
    |> Html.pre []
    |> Element.html
