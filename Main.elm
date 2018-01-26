module Main exposing (main, init, view)

import Color exposing (rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Task
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Environment =
    { width : Float
    , height : Float
    , phone : Bool
    , tablet : Bool
    , desktop : Bool
    , bigDesktop : Bool
    , portrait : Bool
    }


type alias Model =
    { environment : Environment }


type Msg
    = NewSize Window.Size


init : ( Model, Cmd Msg )
init =
    ( Model
        (classifyEnvironment <| Window.Size 1920 1080)
    , Task.perform NewSize Window.size
    )


update :
    Msg
    -> Model
    -> ( Model, Cmd msg )
update msg model =
    case msg of
        NewSize newSize ->
            ( { model
                | environment = classifyEnvironment newSize
              }
            , Cmd.none
            )


subscriptions : a -> Sub Msg
subscriptions model =
    Window.resizes (\windowSize -> NewSize windowSize)



-- Styling


type Styles
    = None
    | Container
    | Panel
    | InfoBox
    | InfoArea
    | CharBox

stylesheet : Environment -> StyleSheet Styles variation
stylesheet env =
    Style.styleSheet
        [ style None []
        , style Container
            [prop "overflow-x" "hidden"]
        , style Panel
            [ Color.background <| rgb 0 0 0
            , Color.text <| rgb 255 255 255
            , Font.size <| env.height * 0.08
            ]
        , style InfoArea
            [ Color.background <| rgb 180 180 180 ]
        , style InfoBox
            [ Border.all 3
            , Font.size <| env.height * 0.025
            , Color.background <| rgb 255 255 255
            , Color.border <| rgb 0 0 0
            , Color.text <| rgb 0 0 0
            ]
        , style CharBox
            [ Color.background <| rgb 0 0 0
            , Color.text <| rgb 255 255 255
            , Font.size  40
            ]
        ]


view : Model -> Html Msg
view model =
    let
        env =
            model.environment
    in
    Element.viewport (stylesheet env) <|
        column Container
            [ width <| px env.width
            , height <| px (env.height * 2)
            ]

            [ homeSection env
            , gallerySection env
            ]

homeSection
    : Environment
    -> Element Styles variation msg
homeSection env =
    let
        -- sizes
        panelHeight =
            env.height * 0.15

        infoAreaHeight =
            env.height - panelHeight

        infoBoxWidth =
            if env.portrait then
                env.width * 0.8
            else
                env.width * 0.4

        infoBoxHeight =
            if env.portrait then
                infoAreaHeight * 0.4
            else
                infoAreaHeight * 0.6

        -- layouts
        container children =
            column None [ width <| px env.width
                        , height <| px env.height
                        ] children

        panel children =
            row Panel
                [ width <| px env.width
                , height <| px panelHeight
                , center
                , verticalCenter
                ]
                children

        infoArea children =
            if env.portrait then
                column InfoArea
                    [ width <| px env.width
                    , height <| px infoAreaHeight
                    , center
                    , verticalSpread
                    , paddingXY 0 (infoAreaHeight * 0.05)
                    ]
                    children
            else
                row InfoArea
                    [ width <| px env.width
                    , height <| px infoAreaHeight
                    , spread
                    , verticalCenter
                    , paddingXY (env.width * 0.05) 0
                    ]
                    children

        infoBox child =
            column InfoBox
                [ width <| px infoBoxWidth
                , height <| px infoBoxHeight
                , center
                , verticalSpread
                , paddingXY (infoBoxWidth * 0.05) (infoBoxHeight * 0.07)
                ]
                child
    in
    container
        [ panel [ text "Title" ]
        , infoArea
            [ infoBox <| quote 1
            , infoBox <| quote 2
            ]
        ]

gallerySection
    : Environment
    -> Element Styles variation msg
gallerySection env =
    wrappedRow None [ spacing 40
                    , center
                    , paddingTop <| env.height * 0.15]
                    <|
        numRow [ 1, 2, 3, 5, 8, 13, 21 ]

numBox : Float -> String -> Element Styles variation msg
numBox hScale char =
    column  CharBox
        [ width <| px 50
        , height <| px (50 * hScale)
        , center
        , verticalCenter
        ]
        [(text char)]

numRow : List Float -> List (Element Styles variation msg)
numRow numList =
    List.map
        (\num ->
            numBox num (toString num)
        )
        numList

-- helpers

classifyEnvironment : Window.Size -> Environment
classifyEnvironment { width, height } =
    { width = toFloat width
    , height = toFloat height
    , phone = width <= 600
    , tablet = width > 600 && width <= 1200
    , desktop = width > 1200 && width <= 1800
    , bigDesktop = width > 1800
    , portrait = width < height
    }



-- content

quote : Int -> List (Element Styles variation msg)
quote id =
    let
        p children =
            paragraph None [] <| [ text children ]
    in
    if id == 1 then
        [ p "\"This soul is full of shadow; sin is therein committed. The guilty one is not the person who has committed the sin, but the person who has created the shadow.\"" ]
    else if id == 2 then
        [ p "\"Why do you deride, do you not see that One spark is needed and it comes to life?"
        , p "But from where you take this, this spark at all?"
        , p "I have to do one step only, not more.\""
        ]
    else
        [ p "ADAM: Great my Lord, Look down and be blush’d: how miserable’s Whom you created to be superb, the man! -"
        , p "THE ESKIMO: Your friend is angry, maybe he’s hungry, too?"
        , p "LUCIFER: Nay, he is angry ’cause he is not hungry."
        ]
