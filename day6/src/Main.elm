{- 

    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden

    https://adventofcode.com/2018/day/6
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
    , biggest : (String, Int)
    , biggestRegion : List PointFloat
    }

initialModel : Model
initialModel =
    { coordinatesText = ""
    , coordinates = []
    , tiles = []
    , labelColors = getLabelColors
    , labelCoordinates = Dict.empty
    , totals = Dict.empty
    , biggest = ("", 0)
    , biggestRegion = []
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
    

-- add100 : Int -> Int
-- add100 value =
--     value + 100

-- add2 : Int -> Int
-- add2 value =
--     value + 2

parseCoordinates : String -> List Coordinate
parseCoordinates string =
    String.split "\n" string
    |> List.map (String.split ",")
    |> List.map (List.map String.trim)
    |> List.map (List.map String.toInt)
    |> List.map (List.map (Maybe.withDefault -4))
    -- |> List.map (List.map add100)
    -- |> List.map (List.map add2)
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
            targetCoordinate = Tuple.second acc
        in
            if distance < currentShortestDistance then
                (distance, coord)
            else
                acc) (200000, coordinate) coordinates


-- NOTE: Return value is a bit complex and I keep forgetting so...
-- coordinate: Coordinate in the row. 0, 0 == coordinate in that particular mdarray.
-- distance: how far is the coordinate from the targetCoordinate. -1 means 0 distance. -2 means it's on the edge and infinite.
-- targetCoordinate: Target Coordinate is the big ole list of coordinates we've parsed from our puzzle input.
calculateCoordinateDistanceForRow : Array Coordinate -> Array Coordinate -> Array (Coordinate, (Int, Coordinate))
calculateCoordinateDistanceForRow row targetCoordinates =
    Array.map (\targetCoordinate -> (targetCoordinate, getShortestDistanceFromAllOtherCoordinates targetCoordinate targetCoordinates)) row
    |> Array.map (\tuples ->
        let
            coordinate = Tuple.first tuples
            distanceAndTarget = Tuple.second tuples
            distance = Tuple.first distanceAndTarget
            target = Tuple.second distanceAndTarget
            datEdge = coordinateTouchesEdge getRows getCols coordinate
        in
        if datEdge == True then
            (coordinate, (-2, target))
        else
            (coordinate, (distance, target)))

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


-- [Challenge 2 Functions] ------------------------------------------------------------

getBlankCoordinate : Coordinate
getBlankCoordinate =
    Coordinate (Point 0 0) (PointFloat (toFloat 0) (toFloat 0)) "blank"

coordinateArrayToSet : Array Coordinate -> Set String
coordinateArrayToSet array =
    Array.foldl (\coordinate set -> Set.insert coordinate.label set) Set.empty array

-- NOTE: This function cost me days; my original was completely wrong so I borrowed what a co-worker had in Python and converted to Elm.
getTopRowSet : Array (Array Coordinate) -> Set String
getTopRowSet mdarray =
    let
        rowLength = (Array.length mdarray) - 1

        firstRow =
            Array.get 0 mdarray
            |> Maybe.withDefault Array.empty
            |> coordinateArrayToSet

        lastRow =
            Array.get rowLength mdarray
            |> Maybe.withDefault Array.empty
            |> coordinateArrayToSet

        firstColumn =
            Array.map (\row -> Array.get 0 row |> Maybe.withDefault getBlankCoordinate ) mdarray
            |> coordinateArrayToSet

        lastColumn =
            Array.map (\row -> Array.get rowLength row |> Maybe.withDefault getBlankCoordinate) mdarray
            |> coordinateArrayToSet

        mergedSet =
            Set.foldl Set.insert firstRow lastRow
            |> Set.foldl Set.insert firstColumn
            |> Set.foldl Set.insert lastColumn
    in
    mergedSet

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
                -- Attempt #8: 4011; all the clever removal of duplicate coordinates weren't so clever... if I just kept it dumb, it worked.
                cols = getCols
                rows = getRows
            
                mdarray =
                    getMDArray cols rows

                coordinatesArray =
                    Array.fromList model.coordinates

                -- calculate all the distances between each pixel
                coordinates = 
                     Array.map (\row -> calculateCoordinateDistanceForRow row coordinatesArray) mdarray

                -- flatten 'em
                coordinateAndDistanceTargets =
                    coordinates
                    |> Array.foldl (\row acc ->
                        Array.foldl (\tuple deepAcc -> Array.push tuple deepAcc) acc row) Array.empty
                
                -- NOTE: Below is what I'm comparing with Python's edges or "infinite"
                -- My version simply calculated if the point was on the sides, but apparently it was super wrong, heh
                infiniteLabels2 =
                    Array.map (\row -> Array.map (\tuple -> Tuple.second(Tuple.second tuple)) row) coordinates
                    |> getTopRowSet
                
                -- filter out the the edges; if it's an edge, it's infinite, so remove all his friends
                finiteDistanceTargets =
                    Array.filter (\item ->
                        let
                            coordinate = Tuple.first item
                            distance = Tuple.first (Tuple.second item)
                            target = Tuple.second (Tuple.second item)
                        in
                            if Set.member target.label infiniteLabels2 then
                                False
                            else
                                True) coordinateAndDistanceTargets
                
                counted = 
                    Dict.empty
                
                -- total up all the coordinate points, and sort by largest first
                totals =
                    finiteDistanceTargets
                    |> Array.foldl (\tuples totalSet ->
                        let
                            closestTarget = Tuple.second (Tuple.second tuples)
                        in
                        updateCountedCoordinateDictionary closestTarget.label totalSet) counted
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.reverse

                biggest =
                    List.head totals
                    |> Maybe.withDefault ("??", 0)

                -- generate tiles; this is strictly for drawing and visualizing the data
                tiles =
                    finiteDistanceTargets
                    |> Array.foldl (\tuples acc ->
                        let
                            coordinate = Tuple.first tuples
                            target = Tuple.second (Tuple.second tuples)
                        in
                         List.append [{coordinate | label = target.label}] acc) []

                -- ** Challenge #2 **
                -- Attempt #1: 210, not sure if this will work, but tired, giving a shot: too low
                -- Attempt #2: 46054, just on a whim to test my count of all items: success! Thank God for David and Python, heh!
                
                -- get all the regions less than 10,000
                region = 
                     Array.map (\row ->
                        Array.map (\coordinate ->
                            (coordinate.pointFloat, Array.foldl (\coord acc -> (getDistanceBetweenCoordinates coordinate coord) + acc) 0 (Array.fromList model.coordinates))) row
                        ) mdarray
                    |> Array.foldl (\row acc -> Array.append row acc) Array.empty
                    |> Array.filter (\tuple -> (Tuple.second tuple) < 10000)

                -- largest region; I re-calculate this in the drawing code so here just for logging/debuggin
                largestRegion = 
                    Array.length region

                biggestRegion =
                    Array.map Tuple.first region
                    |> Array.toList

            in
            { model | tiles = tiles, totals = (Dict.fromList totals), biggest = biggest, biggestRegion = biggestRegion }
            
        ParseCoordinatesText ->
            let
                -- parse the coordinates
                coordinates = parseCoordinates model.coordinatesText
                -- pre-calculate the labels so I can more quickly draw them later
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


-- [Drawing and UI Code] --------------------------------------------------------------------------------
canvasBackgroundColor =
    Color.rgb 26 35 126

getRows : Int
getRows =
    -- 15
    600

getCols : Int
getCols =
    -- 15
    600

getStartX : Float
getStartX =
    -- 20
    560

getBiggestStartX : Float
getBiggestStartX =
    20

getBiggestStartY : Float
getBiggestStartY =
    520


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

formatBiggest : (String, Int) -> String
formatBiggest tuple =
    let
        coordinateLabel = Tuple.first tuple
        amount = String.fromInt (Tuple.second tuple)
        label = coordinateLabel ++ " is biggest area at " ++ amount ++ " points."
    in
    label

drawLineFromBiggestLabelToBiggestCoordinate : Float -> Float -> String -> List Coordinate -> Commands -> Commands
drawLineFromBiggestLabelToBiggestCoordinate x y targetLabel coordinates cmds =
    let
        target =
            List.filter (\coord -> coord.label == targetLabel) coordinates
            |> List.head
            |> Maybe.withDefault getBlankCoordinate
    in
    cmds
    |> Canvas.lineWidth 4
    |> Canvas.strokeStyle (Color.rgb 233 30 99)
    |> Canvas.beginPath
    |> Canvas.moveTo x (y - 14)
    |> Canvas.lineTo target.pointFloat.x target.pointFloat.y
    |> Canvas.stroke

drawBiggest : Float -> Float -> (String, Int) -> List Coordinate -> Commands -> Commands
drawBiggest x y biggest coordinates cmds =
    if Tuple.second biggest > 0 then
        cmds
        |> Canvas.fillStyle (Color.rgb 0 0 0)
        |> Canvas.font "14px Helvetica"
        |> Canvas.fillText (formatBiggest biggest) (x + 1) (y + 1) Nothing
        |> Canvas.fillStyle (Color.rgb 255 255 255)
        |> Canvas.font "14px Helvetica"
        |> Canvas.fillText (formatBiggest biggest) x y Nothing
        |> drawLineFromBiggestLabelToBiggestCoordinate x y (Tuple.first biggest) coordinates
    else
        cmds

getCentroid : List PointFloat -> PointFloat
getCentroid regions =
    let
        length = toFloat (List.length regions)
        accPoint =
            List.foldl (\point acc ->
                PointFloat (acc.x + point.x) (acc.y + point.y)
                ) (PointFloat (toFloat 0) (toFloat 0)) regions
        centerPoint = PointFloat (accPoint.x / length) (accPoint.y / length)
    in
    centerPoint

drawRegions : List PointFloat -> Commands -> Commands
drawRegions regions cmds =
    let
        center = getCentroid regions
    in
    List.foldl (\point acc ->
        acc
        |> Canvas.fillStyle (Color.rgba 233 30 99 0.35)
        |> Canvas.fillRect point.x point.y 1 1
        ) cmds regions

drawBiggestRegion : Float -> Float -> List PointFloat -> Commands -> Commands
drawBiggestRegion x y regions cmds =
    let
        label =
            "Largest region is " ++ String.fromInt (List.length regions) ++ " points."
    in
    if List.length regions > 0 then
        cmds
        |> Canvas.fillStyle (Color.rgb 0 0 0)
            |> Canvas.font "14px Helvetica"
            |> Canvas.fillText label (x + 1) (y + 20 + 1) Nothing
            |> Canvas.fillStyle (Color.rgb 255 255 255)
            |> Canvas.font "14px Helvetica"
            |> Canvas.fillText label x (y + 20) Nothing
            |> drawRegions regions
    else
        cmds

view : Model -> Html Msg
view model =
    div []
        [ div [ class "demo-card-wide mdl-card mdl-shadow--2dp" ]
            [ div [ class "mdl-card__title" ]
                [ h2 [ class "mdl-card__title-text" ] [ Html.text "Day 6 - Coordinates Finder" ]]
                , div[style "width" "100%"][
                    div[class "mdl-grid"][
                        div [class "mdl-cell mdl-cell--4-col"][
                            div [ class "mdl-card__supporting-text" ] [
                                Html.text "1. click 'Load Cached'"
                                , br [] []
                                , Html.text "2. click 'Parse Coordinates'"
                                , br [] []
                                , Html.text "3. click 'Calculate Coordinate Distance'" ]
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
                            ]
                        , div [class "mdl-cell mdl-cell--6-col"][
                        div [][
                            Canvas.element
                                getCols
                                getRows
                                [ style "width" "600px" ]
                                ( Canvas.empty
                                    |> Canvas.clearRect 0 0 getColsFloat getRowsFloat
                                    |> renderBackground getColsFloat getRowsFloat
                                    |> (\cmds -> drawTiles model.tiles model.labelColors cmds)
                                    |> (\cmds -> List.foldl drawCoordinate cmds model.coordinates)
                                    |> (drawLegendFromCoordinates model.labelColors (round getStartX) 0 model.coordinates model.totals)
                                    |> drawCoordinateToLabel model.labelCoordinates model.coordinates
                                    |> drawBiggest getBiggestStartX getBiggestStartY model.biggest model.coordinates
                                    |> drawBiggestRegion getBiggestStartX getBiggestStartY model.biggestRegion
                                )
                            ]
                        ]
                        , div [class "mdl-cell mdl-cell--2-col"][]
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
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick CalculateCoordinateDistance
                    ]
                    [ Html.text "3. Calculate Coordinate Distance" ]

                ]
                
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