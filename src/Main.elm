module Main exposing (..)

import Html exposing (body, br, button, div, form, header, input, label, li, nav, text)
import Html.Attributes exposing (autofocus, disabled, maxlength, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Styles exposing (buttonDisabledStyle, buttonEnabledStyle, centerStyle, centeredBlockStyle, errorStyle, infoTextStyle, marginTopStyle, plateEmptyInputStyle, plateInputStyle, primaryBackgroundStyle, primaryFont, smallMarginBottomStyle, smallMarginTopStyle, styles, subtitleStyle, titleStyle)


type alias Model =
    { plate : String
    , isSearching : Bool
    , failedToGetRatings : Bool
    , failedDetails : String
    , searchResult : SearchResult
    , showResults : Bool
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
    ( Model "" False False "" (SearchResult "" []) False
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
                , failedDetails = toString errorDetails
              }
            , Cmd.none
            )



-- View


validPlate : String -> Bool
validPlate plate =
    String.length plate == 8


resultDiv : Html.Html msg
resultDiv =
    div []
        [ titleDiv
        , div [ styles subtitleStyle ]
            [ Html.text "RESULTS"
            ]
        , div [ styles infoTextStyle ]
            [ Html.text "No results found for this driver yet."
            ]
        , button
            [ styles
                (buttonEnabledStyle
                    ++ smallMarginTopStyle
                )
            ]
            [ Html.text "Add review" ]
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
            "http://localhost:9393/fake"

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
            resultDiv
          else
            searchForm model.plate model.isSearching model.failedToGetRatings model.failedDetails model.searchResult
        ]
