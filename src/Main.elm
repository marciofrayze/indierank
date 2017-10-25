module Main exposing (..)

import Html exposing (body, br, button, div, form, header, input, label, li, nav, text)
import Html.Attributes exposing (autofocus, disabled, maxlength, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Navigation
import Styles exposing (buttonDisabledStyle, buttonEnabledStyle, centerStyle, centeredBlockStyle, errorStyle, infoTextStyle, marginTopStyle, plateEmptyInputStyle, plateInputStyle, primaryBackgroundStyle, primaryFont, smallMarginBottomStyle, smallMarginTopStyle, styles, subtitleStyle, titleStyle)


type alias Model =
    { plate : String
    , isSearching : Bool
    , failedToGetRatings : Bool
    , failedDetails : String
    , searchResult : SearchResult
    , showResults : Bool
    , history : List Navigation.Location
    }


type alias SearchResult =
    { plate : String
    , ratings : List Rating
    }


type alias Rating =
    { score : Int
    , comment : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" False False "" (SearchResult "" []) False []
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = ChangePlate String
    | Search
    | ShowRating (Result Http.Error SearchResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePlate newPlate ->
            ( { model
                | plate = newPlate
                , failedToGetRatings = False
              }
            , Cmd.none
            )

        Search ->
            ( { model
                | isSearching = True
                , failedToGetRatings = False
              }
            , getRatings "abc1234"
            )

        ShowRating (Ok newSearchResult) ->
            ( { model
                | isSearching = False
                , failedToGetRatings = False
                , searchResult = newSearchResult
                , showResults = True
              }
            , Cmd.none
            )

        ShowRating (Err errorDetails) ->
            ( { model
                | isSearching = False
                , failedToGetRatings = True
                , failedDetails = httpErrorString errorDetails
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


validPlate : String -> Bool
validPlate plate =
    String.length plate == 8


resultDiv : SearchResult -> Html.Html msg
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
            [ Html.text (reviewsFoundText (List.length searchResult.ratings))
            ]
        , div [ styles infoTextStyle ]
            (List.map ratingDiv searchResult.ratings)
        , button
            [ styles
                (buttonEnabledStyle
                    ++ smallMarginTopStyle
                )
            ]
            [ Html.text "Add review" ]
        ]


reviewsFoundText : number -> String
reviewsFoundText ratingsSize =
    case ratingsSize of
        1 ->
            "Found 1 review."

        _ ->
            "Found " ++ toString ratingsSize ++ " reviews."


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


searchForm : String -> Bool -> Bool -> String -> SearchResult -> Html.Html Msg
searchForm plate isSearching failedToGetRatings failedDetails searchResult =
    let
        isSearchDisabled =
            not (validPlate plate)

        formatedPlate =
            formatPlate plate
    in
    div []
        [ form
            [ onSubmit Search
            ]
            [ titleDiv
            , div
                [ styles
                    (smallMarginBottomStyle ++ smallMarginTopStyle)
                ]
                [ input
                    [ autofocus True
                    , disabled isSearching
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
            , if isSearching then
                button
                    [ disabled True
                    , styles buttonDisabledStyle
                    ]
                    [ Html.text "searching..." ]
              else if validPlate formatedPlate then
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
            , if failedToGetRatings then
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


getRatings : a -> Cmd Msg
getRatings plate =
    let
        -- TODO: Insert plate on the url
        url =
            "http://10.32.18.57:9393/fake"

        -- Return exemple: {"plate":"ABC-1234","ratings":[{"comment":"really bad driver","score":1}]}
        request =
            Http.get url decodeRatings
    in
    Http.send ShowRating request


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


view : Model -> Html.Html Msg
view model =
    div
        [ styles
            (marginTopStyle ++ centeredBlockStyle)
        ]
        [ if model.showResults then
            resultDiv model.searchResult
          else
            searchForm model.plate model.isSearching model.failedToGetRatings model.failedDetails model.searchResult
        ]
