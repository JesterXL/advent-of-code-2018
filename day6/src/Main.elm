{- 

    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden

    https://adventofcode.com/2018/day/4

-}

module Main exposing (main)

import Array exposing (Array)
import Browser
import Char exposing (fromCode, toCode)
import Debug exposing (log)
import Dict exposing (Dict, update)
import Html exposing (Html, a, br, button, div, form, h2, i, input, label, text, textarea, ul, li, span, p, table, tr, td, thead, tbody, th)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, foldl, head, length, map, partition, reverse)
import Maybe exposing (withDefault)
import Set exposing (Set, empty, insert)
import String exposing (split, toList)
import Tuple exposing (first, pair)
import Canvas exposing (..)
import CanvasColor as Color exposing (Color)

type alias Model =
    { coordinatesText : String
    , coordinates : List Coordinate
    }

initialModel : Model
initialModel =
    { coordinatesText = ""
    , coordinates = []
    }

type Msg
    = InputCoordinatesText String -- when you type or paste in the text area
    | ParseCoordinatesText -- RUN THE MAGIC
    | CalculateCoordinateDistance
    | LoadFromCache -- loads strings vs. you copy pasta

-- [Challenge 1 Functions] ------------------------------------------------------------

type alias Point =
    { x : Int   
    , y : Int }

type alias PointFloat =
    { x : Float
    , y: Float }

type alias Coordinate =
    { point : Point
    , pointFloat : PointFloat
    , label : String }

intListToPoint : List Int -> Point
intListToPoint intList =
    let
        xAndYList = Array.fromList intList
        x = Array.get 0 xAndYList |> Maybe.withDefault 0
        y = Array.get 1 xAndYList |> Maybe.withDefault 0
    in
    Point x y

pointToCoordinate : Int -> Point -> Coordinate
pointToCoordinate index point =
    let
        label = Array.get index getCoordinateLabels |> Maybe.withDefault "?"
    in
        Coordinate point (PointFloat (toFloat point.x) (toFloat point.y)) label
    

add100 : Int -> Int
add100 value =
    value + 100

-- parseCoordinates : String -> List Coordinate
parseCoordinates string =
    String.split "\n" string
    |> List.map (String.split ",")
    |> List.map (List.map String.trim)
    |> List.map (List.map String.toInt)
    |> List.map (List.map (Maybe.withDefault 0))
    |> List.map (List.map add100)
    |> List.map intListToPoint
    |> List.indexedMap pointToCoordinate

{-
    [A, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0]  
  , [0, 0, 0, 0, 0, 0, 0] 
  , [0, 0, 0, 0, 0, 0, 0] 
  , [0, 0, 0, 0, B, 0, 0] 
  , [0, 0, 0, 0, 0, 0, 0] 
  , [0, 0, 0, 0, 0, 0, 0] 
  , [0, 0, 0, 0, 0, 0, 0] 
-}

getManhattanDistance : Point -> Point -> Int
getManhattanDistance point1 point2 =
    let
        -- I have a feeling I may regret this later, but it's a pain passing it down
        size = 1
        x1 = abs point1.x - point2.x
        x2 = abs point2.x - point1.x
        dx = min x1 (size - x2)
        y1 = abs point1.y - point2.y
        y2 = abs point2.y - point1.y
        dy = min y1 (size - y2)
    in
        dx + dy

getDistanceBetweenCoordinates : Coordinate -> Coordinate -> Int
getDistanceBetweenCoordinates coordinate1 coordinate2 =
    getManhattanDistance coordinate1.point coordinate2.point

getCoordinateLabels : Array String
getCoordinateLabels =
    Array.fromList ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
                    , "AA", "BB", "CC", "DD", "EE", "FF", "GG", "HH", "II", "JJ", "KK", "LL", "MM", "NN", "OO", "PP", "QQ", "RR", "SS", "TT", "UU", "VV", "WW", "XX", "YY", "ZZ" ]

-- updateGridFromCoordinate : Coordinate -> Dict String Coordinate -> Dict String Coordinate
-- updateGridFromCoordinate coordinate grid =
--     getXListFromClaim claim
--     |> List.foldl (\x acc->
--             getYListFromClaim claim
--             |> updateGridFromYList x acc) grid

getEmptyRow : Int -> Int -> Array Coordinate
getEmptyRow columnCount row =
    Array.initialize columnCount (\index -> Coordinate (Point index row) (PointFloat (toFloat index)(toFloat row)) "-")

getMDArray : Int -> Int -> Array (Array Coordinate)
getMDArray rowCount columnCount =
    Array.initialize rowCount (\index -> getEmptyRow columnCount index)

calculateCoordinateDistanceForRow : Array Coordinate -> Coordinate -> Array Int
calculateCoordinateDistanceForRow row targetCoordinate =
    Array.map (\coordinate -> getDistanceBetweenCoordinates coordinate targetCoordinate) row


update : Msg -> Model -> Model
update msg model =
    case msg of
        CalculateCoordinateDistance ->
            let
                cols = getCols
                rows = getRows
            
                mdarray =
                    getMDArray rows cols
                    -- |>    
                -- mdarraylog = log "mdarray" mdarray
                coordinates =
                    Array.initialize 10 (\index -> Coordinate (Point index 0) (PointFloat (toFloat index)(toFloat 0)) "-")
                -- coordinateslog = log "coordinates" coordinates
                coordinate2 = Coordinate (Point 4 4) (PointFloat (toFloat 4)(toFloat 4)) "-"
                distance = calculateCoordinateDistanceForRow coordinates coordinate2
                distancelog = log "distance" distance
            in
            model
            
        ParseCoordinatesText ->
            let
                -- ** Challenge 1 **
                coordinates =
                    parseCoordinates model.coordinatesText
                -- coordinatesLog =log "coordinates" coordinates
            in
            { model | coordinates = coordinates }
            -- model

        -- when you type or copy pasta into the text area
        InputCoordinatesText text ->
            { model | coordinatesText = text }

        -- put the sleep strings function text into the text area
        LoadFromCache ->
            { model | coordinatesText = unitsCache }


canvasBackgroundColor =
    Color.rgb 26 35 126

-- canvasCoordinateDotColor =
--     Color.rgb 136 14 79

canvasCoordinateDotColor =
    Color.rgb 233 30 99

canvasNoOverlapColor =
    Color.rgb 252 228 236

-- renderEmptyRectangle x y width height cmds =
--     cmds
--         |> Canvas.lineWidth 3
--         |> Canvas.strokeStyle canvasClaimColor
--         |> Canvas.strokeRect (toFloat x) (toFloat y) (toFloat width) (toFloat height)

-- renderFilledRectangle x y width height color cmds =
--     cmds
--         |> Canvas.fillStyle color
--         |> Canvas.fillRect (toFloat x) (toFloat y) (toFloat width) (toFloat height)

getRows : Int
getRows =
    600

getCols : Int
getCols =
    600

getRowsFloat : Float
getRowsFloat =
    toFloat getRows

getColsFloat : Float
getColsFloat =
    toFloat getCols

renderBackground width height cmds =
    cmds
    |> Canvas.fillStyle canvasBackgroundColor
    |> Canvas.fillRect 0 0 width height

drawCoordinate : Coordinate -> Commands -> Commands
drawCoordinate coordinate cmds =
    cmds
    |> Canvas.fillStyle canvasCoordinateDotColor
    -- |> Canvas.fillCircle coordinate.pointFloat.x coordinate.pointFloat.y 4
        |> Canvas.fillText coordinate.label coordinate.pointFloat.x coordinate.pointFloat.y Nothing

view : Model -> Html Msg
view model =
    div []
        [ div [ class "demo-card-wide mdl-card mdl-shadow--2dp" ]
            [ div [ class "mdl-card__title" ]
                [ h2 [ class "mdl-card__title-text" ] [ Html.text "Day 6 - Coordinates Finder" ]
                ]
                , div [ class "mdl-card__supporting-text" ] [
                    Html.text "1. click 'Load Cached'"
                    , br [] []
                    , Html.text "2. click 'Parse Coordinates'"
                , form [ action "#" ]
                    [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                        [ textarea
                            [ class "mdl-textfield__input"
                            , rows 2
                            , placeholder "Paste claims text here"
                            , required True
                            , onInput InputCoordinatesText
                            ]
                            [ Html.text model.coordinatesText ]
                        ]
                    ]
                    , div [][
                        Canvas.element
                            getCols
                            getRows
                            [ style "border" "1px solid black", style "width" "400px"]
                            ( Canvas.empty
                                |> Canvas.clearRect 0 0 getColsFloat getRowsFloat
                                |> renderBackground getColsFloat getRowsFloat
                                |> (\cmds -> List.foldl drawCoordinate cmds model.coordinates)
                                -- |> Canvas.fillText "Hello world" 50 100 Nothing
                            )
                        ]
                ]   
            , div [ class "mdl-card__actions mdl-card--border" ]
                [ a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick LoadFromCache
                    ]
                    [ Html.text "1. Load Cached" ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick ParseCoordinatesText
                    ]
                    [ Html.text "2. Parse Coordinates" ]
                ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick CalculateCoordinateDistance
                    ]
                    [ Html.text "3. Calculate Coordinate Distance" ]
                
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

unitsCache : String
unitsCache =
    """181, 184
230, 153
215, 179
84, 274
294, 274
127, 259
207, 296
76, 54
187, 53
318, 307
213, 101
111, 71
310, 295
40, 140
176, 265
98, 261
315, 234
106, 57
40, 188
132, 292
132, 312
97, 334
292, 293
124, 65
224, 322
257, 162
266, 261
116, 122
80, 319
271, 326
278, 231
191, 115
277, 184
329, 351
58, 155
193, 147
45, 68
310, 237
171, 132
234, 152
158, 189
212, 100
346, 225
257, 159
330, 112
204, 320
199, 348
207, 189
130, 289
264, 223"""