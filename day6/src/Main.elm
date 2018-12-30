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
    , tiles : List Coordinate
    , labelColors : Dict String Color
    , labelCoordinates : Dict String (Float, Float)
    , totals : Dict String Int
    }

initialModel : Model
initialModel =
    { coordinatesText = ""
    , coordinates = []
    , tiles = []
    , labelColors = getLabelColors
    , labelCoordinates = Dict.empty
    , totals = Dict.empty
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
        x = Array.get 0 xAndYList |> Maybe.withDefault -3
        y = Array.get 1 xAndYList |> Maybe.withDefault -3
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
    |> List.map (List.map (Maybe.withDefault -4))
    |> List.map (List.map add100)
    |> List.map intListToPoint
    |> List.indexedMap pointToCoordinate

getManhattanDistance : Point -> Point -> Int
getManhattanDistance point1 point2 =
    let
        distance = (abs (point2.x - point1.x)) + (abs (point2.y - point1.y))
    in
        distance

getCoordinateLabels : Array String
getCoordinateLabels =
    Array.fromList ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
                    , "AA", "BB", "CC", "DD", "EE", "FF", "GG", "HH", "II", "JJ", "KK", "LL", "MM", "NN", "OO", "PP", "QQ", "RR", "SS", "TT", "UU", "VV", "WW", "XX", "YY", "ZZ" ]

getEmptyRow : Int -> Int -> Array Coordinate
getEmptyRow columnCount row =
    Array.initialize columnCount (\index -> Coordinate (Point index row) (PointFloat (toFloat index)(toFloat row)) "-")

getMDArray : Int -> Int -> Array (Array Coordinate)
getMDArray rowCount columnCount =
    Array.initialize rowCount (\index -> getEmptyRow columnCount index)

getDistanceBetweenCoordinates : Coordinate -> Coordinate -> Int
getDistanceBetweenCoordinates coordinate1 coordinate2 =
    getManhattanDistance coordinate1.point coordinate2.point

coordinatesEqual : Coordinate -> Coordinate -> Bool
coordinatesEqual coordinate1 coordinate2 =
    coordinate1.point.x == coordinate2.point.x && coordinate1.point.y == coordinate2.point.y

getShortestDistanceFromAllOtherCoordinates : Coordinate -> Array Coordinate -> (Int, Coordinate)
getShortestDistanceFromAllOtherCoordinates coordinate coordinates =
    Array.foldl (\coord acc->
        let

            distance = getDistanceBetweenCoordinates coordinate coord
            currentShortestDistance = Tuple.first acc
            -- d1 = log "diff" ("distance: " ++ String.fromInt distance ++ ", currentShortestDistance: " ++ String.fromInt currentShortestDistance)
            targetCoordinate = Tuple.second acc
        in
            -- TODO: need to figure out how to do dots, this doesn't appear to be working
            -- if distance == currentShortestDistance && coordinatesEqual coordinate coord == False then
            if Tuple.first acc == -1 then
                acc
            else if distance == currentShortestDistance then
                -- we've got a Dodson here
                (-1, coord)
            else if distance < currentShortestDistance then
                (distance, coord)
            else
                acc) (20000, coordinate) coordinates

-- NOTE: Return value is a bit complex and I keep forgetting so...
-- coordinate: Coordinate in the row. 0, 0 == coordinate in that particular mdarray.
-- distance: how far is the coordinate from the targetCoordinate. -1 means 0 distance. -2 means it's on the edge and infinite.
-- targetCoordinate: Target Coordinate is the big ole list of coordinates we've parsed from our puzzle input.
calculateCoordinateDistanceForRow : Array Coordinate -> Array Coordinate -> Array (Coordinate, (Int, Coordinate))
calculateCoordinateDistanceForRow row targetCoordinates =
    Array.map (\targetCoordinate -> (targetCoordinate, getShortestDistanceFromAllOtherCoordinates targetCoordinate targetCoordinates)) row
    -- |> Array.slice 0 600
    |> Array.map (\tuples ->
        let
            coordinate = Tuple.first tuples
            distanceAndTarget = Tuple.second tuples
            target = Tuple.second distanceAndTarget
            datEdge = coordinateTouchesEdge getRows getCols coordinate
            -- log1 = log "datEdge" datEdge
        in
        if datEdge == True then
            (coordinate, (-2, target))
        else
            tuples)

updateCountedCoordinateDictionary : String -> Dict String Int -> Dict String Int
updateCountedCoordinateDictionary label dict =
    Dict.update label (\countMaybe ->
        case countMaybe of
            Nothing ->
                Just 1
            Just val ->
                Just (val + 1)) dict


coordinateTouchesEdge : Int -> Int -> Coordinate -> Bool
coordinateTouchesEdge rowCount columnCount coordinate =
    if coordinate.point.x == 0 then
        True
    else if coordinate.point.x == (columnCount - 1) then
        True
    else if coordinate.point.y == 0 then
        True
    else if coordinate.point.y == (rowCount - 1) then
        True
    else
        False

getInfiniteSet : Array (Coordinate, (Int, Coordinate)) -> Set String
getInfiniteSet array =
    Array.foldl (\item acc ->
        let
            coordinate = Tuple.first item
            distanceAndTarget = Tuple.second item
            distance = Tuple.first distanceAndTarget
            target = Tuple.second distanceAndTarget
        in
        if distance == -2 then
            Set.insert target.label acc
        else
        acc) Set.empty array

update : Msg -> Model -> Model
update msg model =
    case msg of
        CalculateCoordinateDistance ->
            let
                -- ** Challenge 1 **
                -- Attempt #1: 545 is too low... uh...
                -- Attempt #2: 546 is too low. I'm in trouble, I thought I was good by this point.
                -- No wait, I'm an idiot, sort is Ascending, DOH
                -- Attempt #3: 4015 is too high. WHAT come on man, I worked hard on this.
                -- Attempt #4: 3822: added back the -1 for same distance, but... not sure this'll work, worth a shot, nope, not right, ugh.
                -- Attempt #5: 3981, heh, failed, grasping at straws at this point. I think my -1 is still good.
                -- Attempt #6: 3819, failed. I corrected my Manhattan Distance function for the 3rd time, heh and I REALLY think it's good this time. However, wrong answer, meh...
                -- Attempt #7: using David's inputs, I get 4186 in his Python. Elm prints out 4122. Yep. *sigh* 
                cols = getCols
                rows = getRows
            
                mdarray =
                    getMDArray cols rows

                coordinatesArray =
                    Array.fromList model.coordinates
                
                coordinateAndDistanceTargets =
                    Array.map (\row -> calculateCoordinateDistanceForRow row coordinatesArray) mdarray
                    |> Array.foldl (\row acc ->
                        Array.foldl (\tuple deepAcc -> Array.push tuple deepAcc) acc row) Array.empty
                
                -- I wonder, does dots that are closest to 2 or more points
                -- count towards making it infinite?
                infiniteLabels =
                    getInfiniteSet coordinateAndDistanceTargets
                    -- Set.empty
                
                finiteDistanceTargets =
                    Array.filter (\item ->
                        let
                            coordinate = Tuple.first item
                            distance = Tuple.first (Tuple.second item)
                            target = Tuple.second (Tuple.second item)
                            -- log1 = log "datmember" (Set.member target.label infiniteLabels)
                        in
                            if Set.member target.label infiniteLabels || distance == -1 then
                                False
                            else
                                True) coordinateAndDistanceTargets
                    

                -- umlog = log "coordinateAndDistanceTargets" coordinateAndDistanceTargets
                -- finiteDistanceTargetslog = log "finiteDistanceTargets" finiteDistanceTargets
                
                counted = 
                    Dict.empty
                
                totals =
                    -- coordinateAndDistanceTargets
                    finiteDistanceTargets
                    |> Array.foldl (\tuples totalSet ->
                        let
                            closestTarget = Tuple.second (Tuple.second tuples)
                        in
                        updateCountedCoordinateDictionary closestTarget.label totalSet) counted
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.reverse
                    -- |> Dict.fromList
                totallog = log "totals" totals

                biggest =
                    List.head totals
                    |> Maybe.withDefault ("??", 0)
                biggestlog = log "biggest" biggest

                tiles =
                    -- coordinateAndDistanceTargets
                    finiteDistanceTargets
                    |> Array.foldl (\tuples acc ->
                        let
                            coordinate = Tuple.first tuples
                            target = Tuple.second (Tuple.second tuples)
                        in
                         List.append [{coordinate | label = target.label}] acc) []
                -- tileslog = log "tiles" tiles
            in
            { model | tiles = tiles, totals = (Dict.fromList totals) }
            
        ParseCoordinatesText ->
            let
                coordinates =
                    parseCoordinates model.coordinatesText
                -- coordinatesLog =log "coordinates" coordinates

                -- coord1 = Coordinate (Point 233 472) (PointFloat (toFloat 233) (toFloat 472)) "a"
                -- coord2 = Coordinate (Point 124 562) (PointFloat (toFloat 124) (toFloat 562)) "b"
                -- dist1 = getDistanceBetweenCoordinates coord1 coord2
                -- log1 = log "dist1" dist1 // 19, now 197, w00t w00000t

                -- log1 = log "log1" (getManhattanDistance (Point 0 0) (Point 0 0)) 
                -- log2 = log "log2" (getManhattanDistance (Point 0 0) (Point 1 0)) 
                -- log3 = log "log3" (getManhattanDistance (Point 0 0) (Point 0 1)) 
                -- log4 = log "log4" (getManhattanDistance (Point 0 0) (Point 1 1)) 
                -- log5 = log "log5" (getManhattanDistance (Point 233 472) (Point 124 562)) 

                labelCoordinates = getLabelCoordinates getStartX 0 coordinates
            in
            { model | coordinates = coordinates, labelCoordinates = labelCoordinates}
            -- model

        -- when you type or copy pasta into the text area
        InputCoordinatesText text ->
            { model | coordinatesText = text }

        -- put the sleep strings function text into the text area
        LoadFromCache ->
            { model | coordinatesText = unitsCache }


canvasBackgroundColor =
    Color.rgb 26 35 126

getRows : Int
getRows =
    600

getCols : Int
getCols =
    600

getStartX : Float
getStartX =
    550

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
    |> Canvas.fillStyle (Color.rgb 0 0 0)
    |> Canvas.fillCircle coordinate.pointFloat.x coordinate.pointFloat.y 4
    -- |> Canvas.fillRect (coordinate.pointFloat.x - 10) (coordinate.pointFloat.y - 7) 20 14
    -- |> Canvas.fillStyle canvasCoordinateDotColor
    -- |> Canvas.font "14px Helvetica"
    -- |> Canvas.fillText coordinate.label (coordinate.pointFloat.x - 8) (coordinate.pointFloat.y + 5) Nothing

plotColors =
    Array.fromList [ Color.rgb 239 83 80
    , Color.rgb 236 64 122
    , Color.rgb 171 71 188
    , Color.rgb 126 87 194
    , Color.rgb 92 107 192
    , Color.rgb 66 165 245
    , Color.rgb 41 182 246
    , Color.rgb 38 198 218
    , Color.rgb 38 166 154
    , Color.rgb 102 187 106
    , Color.rgb 156 204 101
    , Color.rgb 212 225 87
    , Color.rgb 255 238 88
    , Color.rgb 255 202 40
    , Color.rgb 255 167 38
    , Color.rgb 255 112 67
    , Color.rgb 141 110 99
    , Color.rgb 189 189 189
    , Color.rgb 120 144 156
    , Color.rgb 198 40 40
    , Color.rgb 173 20 87
    , Color.rgb 106 27 154
    , Color.rgb 69 39 160
    , Color.rgb 40 53 147
    , Color.rgb 21 101 192
    , Color.rgb 2 119 189
    , Color.rgb 0 131 143
    , Color.rgb 0 105 92
    , Color.rgb 46 125 50
    , Color.rgb 85 139 47
    , Color.rgb 158 157 36
    , Color.rgb 249 168 37
    , Color.rgb 255 143 0
    , Color.rgb 239 108 0
    , Color.rgb 216 67 21
    , Color.rgb 78 52 46
    , Color.rgb 66 66 66
    , Color.rgb 55 71 79
    , Color.rgb 255 23 68
    , Color.rgb 245 0 87
    , Color.rgb 213 0 249
    , Color.rgb 101 31 255
    , Color.rgb 61 90 254
    , Color.rgb 41 121 255
    , Color.rgb 0 176 255
    , Color.rgb 0 229 255
    , Color.rgb 29 233 182
    , Color.rgb 0 230 118
    , Color.rgb 118 255 3
    , Color.rgb 198 255 0
    , Color.rgb 255 234 0
    , Color.rgb 255 196 0
    , Color.rgb 255 145 0
    , Color.rgb 255 61 0
    , Color.rgb 62 39 35
    , Color.rgb 33 33 33
    , Color.rgb 38 50 56 ]

getLabelColors : Dict String Color
getLabelColors =
    let
        colors = plotColors
        labelColorDict =
            Array.indexedMap (\index label ->
                let
                    color = Array.get index colors |> Maybe.withDefault (Color.rgb 0 0 0)
                in
                    (label, color)) getCoordinateLabels
            |> Array.toList
            |> Dict.fromList
    in
    labelColorDict

getLabelCoordinates : Float -> Float -> List Coordinate -> Dict String (Float, Float)
getLabelCoordinates startX startY coordinates =
    List.indexedMap (\index coord -> (startY + (12 * (toFloat index)), coord)) coordinates
    |> List.foldl (\tuple acc ->
        let
            y = Tuple.first tuple
            coord = Tuple.second tuple
        in
            Dict.insert coord.label (startX, y) acc) Dict.empty

drawPlot : Coordinate -> Color -> Commands -> Commands
drawPlot coordinate color cmds =
    cmds
    |> Canvas.fillStyle color
    |> Canvas.fillRect coordinate.pointFloat.x coordinate.pointFloat.y 1 1


getColorElseWhite : String -> Dict String Color -> Color
getColorElseWhite key colors =
    Dict.get key colors
    |> Maybe.withDefault (Color.rgb 255 255 255)

drawTile : Coordinate -> Dict String Color -> Commands -> Commands
drawTile tile labelColorDict cmds =
    cmds
    |> drawPlot tile (getColorElseWhite tile.label labelColorDict)

drawTiles : List Coordinate -> Dict String Color -> Commands -> Commands
drawTiles tiles labelColors cmds =
        List.foldl (\tile acc -> drawTile tile labelColors acc) cmds tiles

drawLegend : Dict String Color -> Int -> Int -> Coordinate -> String -> Commands -> Commands
drawLegend labelColors x y coordinate total cmds =
    let
        color = (getColorElseWhite coordinate.label labelColors)
        xFloat = toFloat x
        yFloat = toFloat y
    in
    cmds
    |> Canvas.fillStyle color
    |> Canvas.fillRect xFloat yFloat 60 11
    |> Canvas.fillStyle (Color.rgb 0 0 0)
    |> Canvas.font "10px Helvetica"
    |> Canvas.fillText (coordinate.label ++ " - " ++ total) (xFloat + 1) (yFloat + 11) Nothing
    |> Canvas.fillStyle (Color.rgb 255 255 255)
    |> Canvas.font "10px Helvetica"
    |> Canvas.fillText (coordinate.label ++ " - " ++ total) xFloat (yFloat + 10) Nothing

-- (\cmds -> List.foldl (drawLegend model.labelColors) cmds model.coordinates)
drawLegendFromCoordinates labelColors startX startY coordinates totals cmds =
    List.indexedMap (\index coord -> (startY + (12 * index), coord)) coordinates
    |> List.foldl (\tuple acc ->
        let
            y = Tuple.first tuple
            coord = Tuple.second tuple
            total = Dict.get coord.label totals |> Maybe.withDefault 0 |> String.fromInt
        in
        acc |> drawLegend labelColors startX y coord total) cmds

drawCoordinateToLabel : Dict String (Float, Float) -> List Coordinate -> Commands -> Commands
drawCoordinateToLabel labelCoordinates coordinates cmds =
    List.foldl (\coordinate acc ->
        let
            tuple = Dict.get coordinate.label labelCoordinates |> Maybe.withDefault (0, 0)
            targetX = Tuple.first tuple
            targetY = Tuple.second tuple
        in
        acc
        |> Canvas.lineWidth 0.5
        |> Canvas.strokeStyle Color.white
        |> Canvas.beginPath
        |> Canvas.moveTo coordinate.pointFloat.x coordinate.pointFloat.y
        |> Canvas.lineTo targetX (targetY + 5)
        |> Canvas.stroke
        ) cmds coordinates
    

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
                            [
                                -- style "border" "1px solid black"
                                -- , style "width" "600px"
                                ]
                            ( Canvas.empty
                                |> Canvas.clearRect 0 0 getColsFloat getRowsFloat
                                |> renderBackground getColsFloat getRowsFloat
                                |> (\cmds -> drawTiles model.tiles model.labelColors cmds)
                                |> (\cmds -> List.foldl drawCoordinate cmds model.coordinates)
                                |> (drawLegendFromCoordinates model.labelColors (round getStartX) 0 model.coordinates model.totals)
                                |> drawCoordinateToLabel model.labelCoordinates model.coordinates
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
    """154, 159
172, 84
235, 204
181, 122
161, 337
305, 104
128, 298
176, 328
146, 71
210, 87
341, 195
50, 96
225, 151
86, 171
239, 68
79, 50
191, 284
200, 122
282, 240
224, 282
327, 74
158, 289
331, 244
154, 327
317, 110
272, 179
173, 175
187, 104
44, 194
202, 332
249, 197
244, 225
52, 127
299, 198
123, 198
349, 75
233, 72
284, 130
119, 150
172, 355
147, 314
58, 335
341, 348
236, 115
185, 270
173, 145
46, 288
214, 127
158, 293
237, 311"""

-- unitsCache : String
-- unitsCache =
--     """181, 184
-- 230, 153
-- 215, 179
-- 84, 274
-- 294, 274
-- 127, 259
-- 207, 296
-- 76, 54
-- 187, 53
-- 318, 307
-- 213, 101
-- 111, 71
-- 310, 295
-- 40, 140
-- 176, 265
-- 98, 261
-- 315, 234
-- 106, 57
-- 40, 188
-- 132, 292
-- 132, 312
-- 97, 334
-- 292, 293
-- 124, 65
-- 224, 322
-- 257, 162
-- 266, 261
-- 116, 122
-- 80, 319
-- 271, 326
-- 278, 231
-- 191, 115
-- 277, 184
-- 329, 351
-- 58, 155
-- 193, 147
-- 45, 68
-- 310, 237
-- 171, 132
-- 234, 152
-- 158, 189
-- 212, 100
-- 346, 225
-- 257, 159
-- 330, 112
-- 204, 320
-- 199, 348
-- 207, 189
-- 130, 289
-- 264, 223"""