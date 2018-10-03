module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)

import Profile exposing (Profile)

-- ROUTING

type Route
    = Home
     | Profile Username
     | Article Slug

parser : Parser (Route -> a ) a
parser =
    oneOf
        [ Parser.map Home Parser.top 
        , Parser.map Profile (s "profile" </> Username.urlParser)
        ]
    
-- PUBLIC HELPERS

href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)

fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

-- INTERNAL

routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []
                Profile username ->
                    [ "profile", Username.toString username ]
                Article slug ->
                    [ "article", Slug.toString slug ]
            in
            "#/" ++ String.join "/" pieces
