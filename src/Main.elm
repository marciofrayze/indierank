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
        , onWithOptions
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
    Model "" "" "" False False "" (SearchResult "" []) False Router.NotFoundRoute


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
    | ShowAbout


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
            , Navigation.newUrl ("/")
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

        ShowAbout ->
            ( { model
                | currentRoute = Router.AboutRoute
              }
            , Navigation.newUrl ("/about")
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
                , requestFailed = False
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
            "Url incorreta: " ++ text

        Http.Timeout ->
            "Tempo limite excedido"

        Http.NetworkError ->
            "Erro na conexão de rede"

        Http.BadStatus response ->
            "Erro no retorno da requisição: " ++ toString response.status.code

        Http.BadPayload message response ->
            "Erro no retorno da requisição: "
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
            "Encontrada 1 classificação."

        _ ->
            "Encontradas " ++ toString ratingsSize ++ " classificações."


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
                    Html.text ("Pontuação média: " ++ toString averageScore)
                  else
                    Html.text "Nenhuma classificação encontrada para este veículo."
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
                [ Html.text "adicionar classificação" ]
            , br [] []
            , button
                [ styles
                    (cancelButtonEnabledStyle
                        ++ smallMarginTopStyle
                        ++ centerStyle
                    )
                , onClick Home
                ]
                [ Html.text "pesquisar novamente" ]
            ]


ratingDiv : Rating -> Html.Html msg
ratingDiv rating =
    div []
        [ Html.text ("Nota: " ++ toString rating.score)
        , div []
            [ Html.text ("Comentário: " ++ toString rating.comment)
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
            [ div [ styles smallMarginBottomStyle ]
                [ Html.text "IndieRank é um sistema independente e anônimo de ranqueamento para o Uber, Cabify, 99Taxi, EasyTaxi e serviços similares."
                ]
            , div [ styles smallMarginBottomStyle ]
                [ Html.text "Infelizmente ainda existem inúmeros casos de assédio de todos os tipos e nem sempre as empresas estão dispostas a tomar alguma atitude. Pensando nisso, criamos este site para tentar compartilhar informações e ajudar a alertar as pessoas para tentar tornar estes serviços de transportes um pouco mais seguros."
                ]
            , div [ styles smallMarginBottomStyle ]
                [ Html.text "Então da próxima vez que for utilizar um destes serviços, ao descobrir a placa do motorista que vai utilizar, entre neste site e consulte seu histórico para saber se alguém já fez algum comentário sobre o mesmo!"
                ]
            , div []
                [ Html.text "E caso tenha feito uma viagem desagradável, compartilhe com o pessoal para que outras pessoas evitem passar pelo mesmo problema ;)"
                ]
            ]
        , button
            [ styles
                (buttonEnabledStyle
                    ++ marginTopStyle
                    ++ centerStyle
                )
            , onClick Home
            ]
            [ Html.text "certo, deixe-me pesquisar" ]
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
                        , placeholder "Digite aqui a placa"
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
                        [ Html.text "pesquisando..." ]
                  else if isValidPlate formatedPlate then
                    button
                        [ disabled False
                        , styles buttonEnabledStyle
                        ]
                        [ Html.text "pesquisar" ]
                  else
                    button
                        [ disabled True
                        , styles buttonDisabledStyle
                        ]
                        [ Html.text "pesquisar" ]
                , div
                    [ styles smallMarginTopStyle
                    ]
                    [ button
                        [ styles buttonEnabledStyle
                        , onWithOptions "click"
                            { stopPropagation = True
                            , preventDefault = True
                            }
                            (Decode.succeed ShowAbout)
                        ]
                        [ Html.text "pra que serve isso?" ]
                    ]
                , if requestFailed then
                    Html.div [ styles errorStyle ]
                        [ Html.text ("Não foi possível obter informações. Detalhes: " ++ failedDetails)
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
                [ onSubmit (SendReview plate reviewScore reviewComment)
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
                            [ Html.text "Selecione uma nota" ]
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
                        , placeholder "Digite aqui seus comentários"
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
                    Html.text "incluindo classificação..."
                  else
                    Html.text "incluir minha classificação"
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
                [ Html.text "deixa pra lá" ]
            , if requestFailed then
                Html.div [ styles errorStyle ]
                    [ Html.text ("Não foi possível obter informações. Detalhes: " ++ failedDetails)
                    ]
              else
                Html.div [ styles errorStyle ]
                    [ Html.text ""
                    ]
            ]


notFoundDiv : Html.Html msg
notFoundDiv =
    div [] [ Html.text "404 - Página não encontrada." ]


reviewAdded : String -> Html.Html Msg
reviewAdded plate =
    div
        [ styles titleStyle
        ]
        [ Html.text "IndieRank"
        , div
            [ styles
                (infoTextStyle
                    ++ marginTopStyle
                )
            ]
            [ Html.text ("Obrigado! Classificação incluída para placa " ++ plate)
            ]
        , button
            [ styles
                (buttonEnabledStyle
                    ++ marginTopStyle
                    ++ centerStyle
                )
            , onClick Home
            ]
            [ Html.text "voltar para página inicial" ]
        ]



-- Services


getRatings : String -> Cmd Msg
getRatings plate =
    let
        url =
            "http://10.32.18.57:9292/search?plate=" ++ plate

        -- Return exemple: {"plate":"ABC-1234","ratings":[{"comment":"really bad driver","score":1}]}
        request =
            Http.get url decodeRatings
    in
        Http.send ShowRating request


postReview : String -> String -> String -> Cmd Msg
postReview plate reviewScore reviewComment =
    let
        url =
            "http://10.32.18.57:9292/add?plate=" ++ plate ++ "&score=" ++ reviewScore ++ "&comment=" ++ reviewComment

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
