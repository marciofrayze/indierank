module Styles exposing (..)

import Css exposing (..)
import Html exposing (body, button, div, header, input, li, nav, node)
import Html.Attributes exposing (attribute, placeholder, type_)


-- Helpers


styles : List Style -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style



-- Colors


primaryColor : Color
primaryColor =
    hex "F04F5E"


secondaryColor : Color
secondaryColor =
    hex "FFFFB3"


primaryDetailsColor : Color
primaryDetailsColor =
    hex "ed4664"


secondaryDetailsColor : Color
secondaryDetailsColor =
    hex "fff"


disabledDetailsColor : Color
disabledDetailsColor =
    hex "D3D3D3"


alertDetailsColor : Color
alertDetailsColor =
    hex "FF9800"



-- Styles


buttonStyle : List Style
buttonStyle =
    [ color secondaryDetailsColor
    , fontSize (px 25)
    , textAlign center
    , padding (px 10)
    , borderRadius (px 5)
    , outline none
    ]


linkStyle : List Style
linkStyle =
    [ color primaryDetailsColor
    , fontSize (px 25)
    , textAlign center
    , padding (px 10)
    , borderRadius (px 5)
    , outline none
    ]


buttonEnabledStyle : List Style
buttonEnabledStyle =
    buttonStyle
        ++ [ backgroundColor primaryDetailsColor
           , borderColor primaryDetailsColor
           ]


cancelButtonEnabledStyle : List Style
cancelButtonEnabledStyle =
    buttonStyle
        ++ [ backgroundColor alertDetailsColor
           , borderColor primaryDetailsColor
           ]


buttonDisabledStyle : List Style
buttonDisabledStyle =
    buttonEnabledStyle
        ++ [ backgroundColor disabledDetailsColor
           , borderColor disabledDetailsColor
           , color primaryColor
           ]


primaryBackgroundStyle : List Style
primaryBackgroundStyle =
    [ backgroundColor secondaryColor
    ]


titleStyle : List Style
titleStyle =
    [ fontSize (px 55)
    , paddingBottom (px 40)
    , color primaryColor
    ]
        ++ primaryFont


infoTextStyle : List Style
infoTextStyle =
    [ fontSize (px 20)
    , paddingTop (px 20)
    , color primaryDetailsColor
    ]
        ++ primaryFont


subtitleStyle : List Style
subtitleStyle =
    [ fontSize (px 30)
    , paddingTop (px 30)
    , paddingBottom (px 30)
    , color primaryColor
    ]
        ++ primaryFont


errorStyle : List Style
errorStyle =
    [ fontSize (px 18)
    , marginTop (px 10)
    , color primaryColor
    ]
        ++ primaryFont


centerStyle : List Style
centerStyle =
    [ textAlign center
    , verticalAlign middle
    ]


blockStyle : List Style
blockStyle =
    [ display block ]


centeredBlockStyle : List Style
centeredBlockStyle =
    blockStyle ++ centerStyle


smallMarginBottomStyle : List Style
smallMarginBottomStyle =
    [ marginBottom (px 20) ]


smallMarginTopStyle : List Style
smallMarginTopStyle =
    [ marginTop (px 20) ]


marginTopStyle : List Style
marginTopStyle =
    [ marginTop (px 70) ]


plateInputStyle : List Style
plateInputStyle =
    [ height (px 40)
    , width (px 195)
    , padding (px 15)
    , fontSize (px 40)
    , outline none
    ]


starsSelectStyle : List Style
starsSelectStyle =
    [ height (px 50)
    , width (px 310)
    , padding (px 2)
    , fontSize (px 32)
    , outline none
    , textAlignLast center
    ]



-- TODO: create generic and mobile first input style


reviewInputStyle : List Style
reviewInputStyle =
    [ height (px 40)
    , width (pct 70)
    , padding (px 15)
    , fontSize (px 40)
    , outline none
    ]


plateEmptyInputStyle : List Style
plateEmptyInputStyle =
    [ fontSize (px 22)
    ]


primaryFont : List Style
primaryFont =
    [ fontFamilies [ "Georgia", "serif" ]
    , fontWeight bold
    ]
