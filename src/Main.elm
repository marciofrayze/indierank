module Main exposing (..)

import Html exposing (body, br, button, div, form, header, input, label, li, nav, text)
import Html.Attributes exposing (autofocus, disabled, maxlength, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Styles exposing (buttonDisabledStyle, buttonEnabledStyle, centerStyle, centeredBlockStyle, errorStyle, marginTopStyle, plateEmptyInputStyle, plateInputStyle, primaryBackgroundStyle, primaryFont, smallMarginBottomStyle, smallMarginTopStyle, styles, titleStyle)


type alias Model =
    { plate : String
    , isSearching : Bool
    , failedToGetRatings : Bool
    , failedDetails : String
    , searchResult : List String
    }


type alias Rating =
    { score : Int
    , comment : String
    }


type alias SearchResult =
    { plate : String
    , ratings : List Rating
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" False False "" []
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
    | ShowRating (Result Http.Error (List String))


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


searchForm : String -> Bool -> Bool -> String -> List String -> Html.Html Msg
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
            [ div
                [ styles titleStyle
                ]
                [ Html.text "IndieRank"
                ]
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
        , Html.div []
            [ Html.text (toString searchResult) ]
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

        request =
            Http.get url decodeRatingUrl
    in
    Http.send ShowRating request


decodeRatingUrl : Decode.Decoder (List String)
decodeRatingUrl =
    Decode.field "comments" (Decode.list Decode.string)


view : Model -> Html.Html Msg
view model =
    div
        [ styles
            (marginTopStyle ++ centeredBlockStyle)
        ]
        [ searchForm model.plate model.isSearching model.failedToGetRatings model.failedDetails model.searchResult ]
