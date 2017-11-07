module Main exposing (..)

import Html
    exposing
        ( body
        , br
        , button
        , div
        , form
        , header
        , input
        , label
        , li
        , nav
        , text
        )
import Html.Attributes
    exposing
        ( autofocus
        , disabled
        , href
        , maxlength
        , placeholder
        , value
        )
import Html.Events
    exposing
        ( onInput
        , onSubmit
        , onClick
        )
import Http
import Json.Decode as Decode
import Navigation exposing (Location)
import Styles
    exposing
        ( buttonDisabledStyle
        , buttonEnabledStyle
        , centerStyle
        , centeredBlockStyle
        , errorStyle
        , infoTextStyle
        , marginTopStyle
        , plateEmptyInputStyle
        , plateInputStyle
        , primaryBackgroundStyle
        , primaryFont
        , smallMarginBottomStyle
        , smallMarginTopStyle
        , styles
        , subtitleStyle
        , titleStyle
        , linkStyle
        , blockStyle
        , reviewInputStyle
        , cancelButtonEnabledStyle
        , starsSelectStyle
        )
import Router


type alias Model =
    { plate : String
    , reviewScore : String
    , reviewComment : String
    , processingRequest : Bool
    , requestFailed : Bool
    , failedDetails : String
    , searchResult : SearchResult
    , showResults : Bool
    , history : List Navigation.Location
    , currentRoute : Router.Route
    }



-- Add Review Service return


type alias AddReviewResult =
    { plate : String
    , score : Int
    , comment : String
    }



-- Search Service return


type alias SearchResult =
    { plate : String
    , ratings : List Rating
    }


type alias Rating =
    { score : Int
    , comment : String
    }



-- Program


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


defaultModel : Model
defaultModel =
    Model "" "" "" False False "" (SearchResult "" []) False [] Router.NotFoundRoute


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    stateBasedOnURL location defaultModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = ChangePlate String
    | ChangeReviewScore String
    | ChangeReviewComment String
    | Search String
    | UrlChange Navigation.Location
    | Home
    | AddReview String
    | SendReview String String String -- TODO: Create a type?
    | ShowRating (Result Http.Error SearchResult)
    | ShowReviewAdded (Result Http.Error AddReviewResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePlate newPlate ->
            ( { model
                | plate = newPlate
                , requestFailed = False
              }
            , Cmd.none
            )

        Search plate ->
            ( { model
                | processingRequest = True
                , requestFailed = False
              }
            , getRatings plate
            )

        UrlChange newLocation ->
            stateBasedOnURL newLocation model

        ShowRating (Ok newSearchResult) ->
            ( { model
                | processingRequest = False
                , requestFailed = False
                , searchResult = newSearchResult
                , showResults = True
              }
            , Navigation.newUrl ("/plate/" ++ newSearchResult.plate)
            )

        ShowRating (Err errorDetails) ->
            ( { model
                | processingRequest = False
                , requestFailed = True
                , failedDetails = httpErrorString errorDetails
              }
            , Cmd.none
            )

        ShowReviewAdded (Ok newAddReviewResult) ->
            ( { model
                | processingRequest = False
                , requestFailed = False
              }
            , Navigation.newUrl ("/added/" ++ newAddReviewResult.plate)
            )

        ShowReviewAdded (Err errorDetails) ->
            ( { model
                | processingRequest = False
                , requestFailed = True
                , failedDetails = httpErrorString errorDetails
              }
            , Cmd.none
            )

        Home ->
            ( { model
                | processingRequest = False
                , requestFailed = False
                , failedDetails = ""
                , plate = ""
                , currentRoute = Router.SearchRoute
              }
            , Cmd.none
            )

        AddReview plate ->
            ( { model
                | plate = plate
                , currentRoute = Router.AddReviewRoute plate
              }
            , Navigation.newUrl ("/review/" ++ plate)
            )

        SendReview plate reviewScore reviewComment ->
            ( { model
                | processingRequest = True
              }
            , postReview plate reviewScore reviewComment
            )

        ChangeReviewScore reviewScore ->
            ( { model
                | reviewScore = reviewScore
              }
            , Cmd.none
            )

        ChangeReviewComment reviewComment ->
            ( { model
                | reviewComment = reviewComment
              }
            , Cmd.none
            )


stateBasedOnURL : Location -> Model -> ( Model, Cmd msg )
stateBasedOnURL location model =
    case (Router.parseLocation location) of
        Router.SearchRoute ->
            ( { model
                | currentRoute = Router.SearchRoute
                , requestFailed = False
              }
            , Cmd.none
            )

        Router.SearchResultRoute plate ->
            ( { model
                | currentRoute = Router.SearchResultRoute plate
                , requestFailed = False
                , plate = plate
              }
            , Cmd.none
            )

        Router.AboutRoute ->
            ( { model
                | currentRoute = Router.AboutRoute
                , requestFailed = True
                , failedDetails = toString (Router.parseLocation location)
              }
            , Cmd.none
            )

        Router.AddReviewRoute plate ->
            ( { model
                | currentRoute = Router.AddReviewRoute plate
                , plate = plate
              }
            , Cmd.none
            )

        Router.ReviewAdded plate ->
            ( { model
                | currentRoute = Router.ReviewAdded plate
                , plate = plate
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | processingRequest = False
                , requestFailed = False
                , failedDetails = ""
                , currentRoute = Router.NotFoundRoute
              }
            , Cmd.none
            )


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.status.code

        Http.BadPayload message response ->
            "Bad Http Payload: "
                ++ toString message
                ++ " ("
                ++ toString response.status.code
                ++ ")"



-- View


isValidPlate : String -> Bool
isValidPlate plate =
    String.length plate == 8


reviewsFoundText : number -> String
reviewsFoundText ratingsSize =
    case ratingsSize of
        1 ->
            "Found 1 review."

        _ ->
            "Found " ++ toString ratingsSize ++ " reviews."


isValidReview : String -> String -> Bool
isValidReview score description =
    (List.member score [ "1", "2", "3", "4", "5" ]
        && (String.length (String.trim description) > 5)
    )


view : Model -> Html.Html Msg
view model =
    div
        [ styles
            (marginTopStyle ++ centeredBlockStyle)
        ]
        [ case model.currentRoute of
            Router.SearchRoute ->
                searchForm
                    model.plate
                    model.processingRequest
                    model.requestFailed
                    model.failedDetails
                    model.searchResult

            Router.SearchResultRoute plate ->
                resultDiv model.searchResult

            Router.AddReviewRoute plate ->
                addReview plate model.reviewScore model.reviewComment model.processingRequest model.requestFailed model.failedDetails

            Router.AboutRoute ->
                aboutPage

            Router.ReviewAdded plate ->
                reviewAdded plate

            Router.NotFoundRoute ->
                notFoundDiv
        ]


resultDiv : SearchResult -> Html.Html Msg
resultDiv searchResult =
    let
        ratingsLength =
            toFloat (List.length searchResult.ratings)

        averageScore =
            -- TODO: Any better way to sum this?
            toFloat (List.sum (List.map (\r -> r.score) searchResult.ratings)) / ratingsLength

        containsReviews =
            ratingsLength > 0
    in
        div []
            [ titleDiv
            , div [ styles subtitleStyle ]
                [ Html.text searchResult.plate
                ]
            , div [ styles infoTextStyle ]
                [ if containsReviews then
                    Html.text ("Average score: " ++ toString averageScore)
                  else
                    Html.text "No results found for this driver yet."
                ]
            , div
                [ styles infoTextStyle ]
                [ if containsReviews then
                    Html.text (reviewsFoundText (List.length searchResult.ratings))
                  else
                    Html.text ""
                ]
            , div [ styles infoTextStyle ]
                (List.map ratingDiv searchResult.ratings)
            , button
                [ styles
                    (buttonEnabledStyle
                        ++ smallMarginTopStyle
                    )
                , onClick (AddReview searchResult.plate)
                ]
                [ Html.text "add review" ]
            , br [] []
            , button
                [ styles
                    (cancelButtonEnabledStyle
                        ++ smallMarginTopStyle
                        ++ centerStyle
                    )
                , onClick Home
                ]
                [ Html.text "another search" ]
            ]


ratingDiv : Rating -> Html.Html msg
ratingDiv rating =
    div []
        [ Html.text rating.comment
        , div []
            [ Html.text ("Score: " ++ toString rating.score)
            ]
        ]


titleDiv : Html.Html msg
titleDiv =
    div
        [ styles titleStyle
        ]
        [ Html.text "IndieRank"
        ]


aboutPage : Html.Html Msg
aboutPage =
    div
        []
        [ titleDiv
        , div [ styles infoTextStyle ]
            [ Html.text "IndieRank is an independent ranking system for Uber, Cabify, 99Taxi, EasyTaxi and alikes."
            ]
        , button
            [ styles
                (buttonEnabledStyle
                    ++ smallMarginTopStyle
                    ++ centerStyle
                )
            , onClick Home
            ]
            [ Html.text "ok, let me search" ]
        ]


searchForm : String -> Bool -> Bool -> String -> SearchResult -> Html.Html Msg
searchForm plate processingRequest requestFailed failedDetails searchResult =
    let
        formatedPlate =
            formatPlate plate
    in
        div []
            [ titleDiv
            , form
                [ onSubmit (Search plate)
                ]
                [ div
                    [ styles
                        (smallMarginBottomStyle ++ smallMarginTopStyle)
                    ]
                    [ input
                        [ autofocus True
                        , disabled processingRequest
                        , placeholder "Type the plate here"
                        , onInput ChangePlate
                        , maxlength 8
                        , styles plateInputStyle
                        , value formatedPlate
                        , if String.isEmpty formatedPlate then
                            styles plateEmptyInputStyle
                          else
                            styles []
                        ]
                        []
                    ]
                , if processingRequest then
                    button
                        [ disabled True
                        , styles buttonDisabledStyle
                        ]
                        [ Html.text "searching..." ]
                  else if isValidPlate formatedPlate then
                    button
                        [ disabled False
                        , styles buttonEnabledStyle
                        ]
                        [ Html.text "search" ]
                  else
                    button
                        [ disabled True
                        , styles buttonDisabledStyle
                        ]
                        [ Html.text "search" ]
                , if requestFailed then
                    Html.div [ styles errorStyle ]
                        [ Html.text ("Failed to retrieve data. Details: " ++ failedDetails)
                        ]
                  else
                    Html.div [ styles errorStyle ]
                        [ Html.text ""
                        ]
                ]
            ]


formatPlate : String -> String
formatPlate plate =
    let
        validPlateChars =
            "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm"

        validPlateNumbers =
            "1234567890"

        isValidPlateChar char =
            String.contains (String.fromChar char) validPlateChars

        isValidPlateNumber char =
            String.contains (String.fromChar char) validPlateNumbers

        filteredPlateChars =
            String.filter isValidPlateChar (String.slice 0 3 plate)

        filteredPlateNumbers =
            String.filter isValidPlateNumber (String.slice 3 8 plate)
    in
        if String.length (filteredPlateChars ++ filteredPlateNumbers) > 3 then
            String.toUpper filteredPlateChars ++ "-" ++ filteredPlateNumbers
        else
            String.toUpper filteredPlateChars


addReview : String -> String -> String -> Bool -> Bool -> String -> Html.Html Msg
addReview plate reviewScore reviewComment processingRequest requestFailed failedDetails =
    let
        validReview =
            isValidReview reviewScore reviewComment
    in
        div [ styles centerStyle ]
            [ titleDiv
            , form
                [ onSubmit Home
                , disabled processingRequest
                ]
                [ div [ styles subtitleStyle ]
                    [ Html.text plate
                    ]
                , div [ styles (centerStyle) ]
                    [ Html.select
                        [ styles
                            (starsSelectStyle
                                ++ smallMarginTopStyle
                            )
                        , onInput ChangeReviewScore
                        ]
                        [ Html.option
                            []
                            [ Html.text "Select score" ]
                        , Html.option []
                            [ Html.text "1" ]
                        , Html.option []
                            [ Html.text "2" ]
                        , Html.option []
                            [ Html.text "3" ]
                        , Html.option []
                            [ Html.text "4" ]
                        , Html.option []
                            [ Html.text "5" ]
                        ]
                    ]
                , div
                    [ styles
                        (smallMarginBottomStyle
                            ++ smallMarginTopStyle
                        )
                    ]
                    [ input
                        [ autofocus True
                        , placeholder "Type review here"
                        , onInput ChangePlate
                        , maxlength 245
                        , styles reviewInputStyle
                        , onInput ChangeReviewComment
                        ]
                        []
                    ]
                ]
            , button
                [ onClick (SendReview plate reviewScore reviewComment)
                , if
                    (not validReview
                        || processingRequest
                    )
                  then
                    styles buttonDisabledStyle
                  else
                    styles buttonEnabledStyle
                , disabled (not validReview)
                ]
                [ if processingRequest then
                    Html.text "adding review..."
                  else
                    Html.text "add my review"
                ]
            , br [] []
            , button
                [ if processingRequest then
                    styles
                        (buttonDisabledStyle
                            ++ smallMarginTopStyle
                            ++ centerStyle
                        )
                  else
                    styles
                        (cancelButtonEnabledStyle
                            ++ smallMarginTopStyle
                            ++ centerStyle
                        )
                , onClick Home
                ]
                [ Html.text "never mind" ]
            , if requestFailed then
                Html.div [ styles errorStyle ]
                    [ Html.text ("Failed to retrieve data. Details: " ++ failedDetails)
                    ]
              else
                Html.div [ styles errorStyle ]
                    [ Html.text ""
                    ]
            ]


notFoundDiv : Html.Html msg
notFoundDiv =
    div [] [ Html.text "404 - Page not found." ]


reviewAdded : String -> Html.Html Msg
reviewAdded plate =
    div
        [ styles titleStyle
        ]
        [ Html.text "IndieRank"
        , div [ styles infoTextStyle ]
            [ Html.text ("Thanks! Review added for plate " ++ plate)
            ]
        , button
            [ styles
                (buttonEnabledStyle
                    ++ smallMarginTopStyle
                    ++ centerStyle
                )
            , onClick Home
            ]
            [ Html.text "ok, let me search" ]
        ]



-- Services


getRatings : String -> Cmd Msg
getRatings plate =
    let
        url =
            "http://10.32.18.57:9393?plate=" ++ plate

        -- Return exemple: {"plate":"ABC-1234","ratings":[{"comment":"really bad driver","score":1}]}
        request =
            Http.get url decodeRatings
    in
        Http.send ShowRating request


postReview : String -> String -> String -> Cmd Msg
postReview plate reviewScore reviewComment =
    let
        url =
            "http://10.32.18.57:9393/add?plate=" ++ plate ++ "&score=" ++ reviewScore ++ "&comment=" ++ reviewComment

        request =
            Http.post url Http.emptyBody decodePostReview
    in
        Http.send ShowReviewAdded request


decodeRatings : Decode.Decoder SearchResult
decodeRatings =
    Decode.map2
        SearchResult
        (Decode.at [ "plate" ] Decode.string)
        (Decode.field "ratings" (Decode.list decodeRating))


decodeRating : Decode.Decoder Rating
decodeRating =
    Decode.map2
        Rating
        (Decode.at [ "score" ] Decode.int)
        (Decode.at [ "comment" ] Decode.string)


decodePostReview : Decode.Decoder AddReviewResult
decodePostReview =
    Decode.map3
        AddReviewResult
        (Decode.at [ "plate" ] Decode.string)
        (Decode.at [ "score" ] Decode.int)
        (Decode.at [ "comment" ] Decode.string)
