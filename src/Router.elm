module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = SearchRoute
    | SearchResultRoute String
    | NotFoundRoute
    | AboutRoute
    | AddReviewRoute String
    | ReviewAdded String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map SearchRoute top
        , map AboutRoute (s "about")
        , map SearchResultRoute (s "plate" </> string)
        , map AddReviewRoute (s "review" </> string)
        , map ReviewAdded (s "added" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
