module Main exposing (main)

import Arithmetic
import Browser
import Element as E exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = E.layout [] << view
        , update = update
        }


type alias Model =
    { a : Int
    , b : Int
    }


type Msg
    = SetA Int
    | SetB Int


init : Model
init =
    { a = 1
    , b = 1
    }


type alias Step =
    { a : Int
    , b : Int
    , q : Int
    , r : Int
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetA a ->
            { model | a = a }

        SetB b ->
            { model | b = b }


view : Model -> Element Msg
view ({ a, b } as model) =
    let
        steps =
            euclidSteps model
    in
    E.row
        [ E.width E.fill
        , E.height E.fill
        , E.spacing 20
        , E.padding 20
        ]
        [ E.column
            [ E.alignTop
            , E.spacing 10
            , E.width (E.px 300)
            , Font.size 16
            ]
            [ E.paragraph []
                [ E.link []
                    { url = "https://en.wikipedia.org/wiki/Euclidean_algorithm"
                    , label =
                        E.el [ Font.color (E.rgb255 114 159 207), Font.size 30 ]
                            (E.text "Euclidean algorithm")
                    }
                , E.text " allows you to compute greatest common divisor (gcd) of two numbers. Pick two numbers using the sliders below."
                ]
            , slider SetA " a = " a
            , slider SetB " b = " b
            , E.el [] (E.text "Steps of the algorithm")
            , viewSteps steps
            , E.row []
                [ E.text <|
                    "gcd ("
                        ++ String.fromInt a
                        ++ ", "
                        ++ String.fromInt b
                        ++ ") = "
                , E.el
                    [ Font.color <|
                        Maybe.withDefault (E.rgb255 0 0 0) <|
                            List.head <|
                                List.drop (List.length steps - 1) colors
                    ]
                  <|
                    E.text <|
                        String.fromInt <|
                            Arithmetic.gcd a b
                ]
            ]
        , E.el [ Border.width 1, E.alignTop ]
            (rectangle (a < b) <| List.map2 Tuple.pair steps colors)
        ]


slider : (Int -> Msg) -> String -> Int -> Element Msg
slider toMsg labelPrefix val =
    Input.slider
        [ E.width (E.px 200)
        , E.behindContent <|
            E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , Background.color (E.rgb255 200 200 200)
                , Border.rounded 2
                ]
                E.none
        ]
        { onChange = toMsg << round
        , label =
            Input.labelRight []
                (E.text <| labelPrefix ++ String.fromInt val)
        , min = 1
        , max = 100
        , value = toFloat val
        , thumb = Input.defaultThumb
        , step = Just 1
        }


viewSteps : List Step -> Element msg
viewSteps steps =
    E.column [] (List.map2 viewStep steps (triples (E.rgb255 0 0 0 :: colors)))


viewStep : Step -> ( Color, Color, Color ) -> Element msg
viewStep { a, b, q, r } ( aColor, bColor, rColor ) =
    let
        num =
            E.text << String.padLeft 2 ' ' << String.fromInt
    in
    E.row []
        [ E.el [ Font.color aColor ] (num a)
        , E.text " = "
        , num q
        , E.text " * "
        , E.el [ Font.color bColor ] (num b)
        , E.text " + "
        , E.el [ Font.color rColor ] (num r)
        ]


euclidSteps : Model -> List Step
euclidSteps { a, b } =
    let
        bigger =
            max a b

        smaller =
            min a b

        euclidStepsHelp : Int -> Int -> List Step -> List Step
        euclidStepsHelp x y acc =
            let
                q =
                    x // y

                r =
                    modBy y x

                newAcc =
                    { a = x
                    , b = y
                    , q = q
                    , r = r
                    }
                        :: acc
            in
            if r > 0 then
                euclidStepsHelp y r newAcc

            else
                newAcc
    in
    List.reverse <| euclidStepsHelp bigger smaller []


rectangle : Bool -> List ( Step, Color ) -> Element msg
rectangle isColumn stepsAndColors =
    let
        ( cellRow, largerDim, smallerDim ) =
            if isColumn then
                ( E.column, E.height, E.width )

            else
                ( E.row, E.width, E.height )
    in
    case stepsAndColors of
        [] ->
            E.none

        ( { a, b, q }, color ) :: rest ->
            cellRow
                [ smallerDim <| E.px <| 10 * b
                , largerDim <| E.px <| 10 * a
                ]
                (List.repeat q (square color b)
                    ++ [ rectangle (not isColumn) rest ]
                )


square : Color -> Int -> Element msg
square color size =
    E.el
        [ E.width <| E.px <| 10 * size
        , E.height <| E.px <| 10 * size
        , Border.width 1
        , Background.color color
        ]
        E.none


colors : List Color
colors =
    -- TODO nicer color palette
    [ E.rgb255 255 0 0
    , E.rgb255 0 127 0
    , E.rgb255 0 0 255
    , E.rgb255 255 72 0
    , E.rgb255 255 0 255
    , E.rgb255 16 101 101
    , E.rgb255 127 0 0
    , E.rgb255 0 255 0
    , E.rgb255 0 0 127
    , E.rgb255 127 127 127
    ]


triples : List a -> List ( a, a, a )
triples xs =
    case xs of
        a :: b :: c :: rest ->
            ( a, b, c ) :: triples (b :: c :: rest)

        _ ->
            []
