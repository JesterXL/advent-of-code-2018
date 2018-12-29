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
    }

initialModel : Model
initialModel =
    { coordinatesText = ""
    , coordinates = []
    , tiles = []
    , labelColors = getLabelColors
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
        size = 1 -- I have a feeling I may regret this later, but it's a pain passing it down
        x1 = abs (point1.x - point2.x)
        x2 = abs (point2.x - point1.x)
        dx = min x1 (size - x2)
        y1 = abs (point1.y - point2.y)
        y2 = abs (point2.y - point1.y)
        dy = min y1 (size - y2)
        distance = abs (dx + dy)

        -- point1log = log "point1" point1
        -- point2log = log "point2" point2
        -- x1log = log "x1" x1
        -- x2log = log "x2" x2
        -- dxlog = log "dx" dx
        -- y1log = log "y1" y1
        -- y2log = log "y2" y2
        -- dylog = log "dy" dy
        -- distancelog = log "distance" distance
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

-- TODO: I think modifying this function to return coord instead of coordinate gives you the
-- distance too the target coordinate, but it removes "who" the original coordinate is. You need both.
-- we should probably return a Dictionary instead of a Tuple.
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
            if distance == currentShortestDistance then
                -- we've got a dodson here
                (-1, coord)
            else if distance < currentShortestDistance then
                (distance, coord)
            else
                acc) (20000, coordinate) coordinates

-- NOTE: Return value is a bit complex and I keep forgetting so...
-- coordinate: Coordinate in the row. 0, 0 == coordinate in that particular mdarray.
-- distance: how far is the coordinate from the targetCoordinate. -1 means 0 distance. -2 means it's on the edge and infinite.
-- targetCoordinate: Target Coordinate is the big ole list of coordinates we've parsed from our puzzle input.
-- calculateCoordinateDistanceForRow : Array Coordinate -> Array Coordinate -> Array String
calculateCoordinateDistanceForRow row targetCoordinates =
    Array.map (\targetCoordinate -> (targetCoordinate, getShortestDistanceFromAllOtherCoordinates targetCoordinate targetCoordinates)) row
    -- |> Array.slice 0 600
    |> Array.map (\tuples ->
        let
            coordinate = Tuple.first tuples
            distanceAndTarget = Tuple.second tuples
            target = Tuple.second distanceAndTarget
            datEdge = coordinateTouchesEdge 600 600 coordinate
            -- log1 = log "datEdge" datEdge
        in
        if datEdge == True then -- moarrrr hardcoding regrets
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
                cols = getCols
                rows = getRows
            
                mdarray =
                    getMDArray cols rows
                    -- |>    
                -- mdarraylog = log "mdarray" mdarray

                -- coordinates =
                --     Array.initialize 10 (\index -> Coordinate (Point index 0) (PointFloat (toFloat index)(toFloat 0)) "-")
                -- -- coordinateslog = log "coordinates" coordinates
                -- coordinate2 = Coordinate (Point 4 4) (PointFloat (toFloat 4)(toFloat 4)) "-"
                -- distance = calculateCoordinateDistanceForRow coordinates coordinate2
                -- distancelog = log "distance" distance

                -- row =
                --     Array.initialize 10 (\index -> Coordinate (Point index 0) (PointFloat (toFloat index)(toFloat 0)) "-")
                
                -- wat = calculateCoordinateDistanceForRow row (Array.fromList model.coordinates)
                -- watlog = log "wat" wat

                coordinatesArray =
                    Array.fromList model.coordinates
                
                coordinateAndDistanceTargets =
                    Array.map (\row -> calculateCoordinateDistanceForRow row coordinatesArray) mdarray
                    |> Array.foldl (\row acc ->
                        Array.foldl (\tuple deepAcc -> Array.push tuple deepAcc) acc row) Array.empty
                        
                -- NOTE: I give up on infinite for now, lol, this is hard... it keeps getting
                -- every single tile as somehow having at least 1 on the edge. This means
                -- that technically every area is infinite which can't be correct, 
                -- else my puzzle input is bugged like they said Day 6 had a bug.
                -- I'm betting on my crappy math skills being the problem.

                -- infiniteLabels =
                --     getInfiniteSet coordinateAndDistanceTargets
                -- infiniteLabelslog = log "infiniteLabels" infiniteLabels
                -- finiteDistanceTargets =
                --     Array.filter (\item ->
                --         let
                --             coordinate = Tuple.first item
                --             target = Tuple.second (Tuple.second item)
                --             -- log1 = log "datmember" (Set.member target.label infiniteLabels)
                --         in
                --             if Set.member target.label infiniteLabels then
                --                 False
                --             else
                --                 True) coordinateAndDistanceTargets
                --     |> Array.length

                -- umlog = log "coordinateAndDistanceTargets" coordinateAndDistanceTargets
                -- finiteDistanceTargetslog = log "finiteDistanceTargets" finiteDistanceTargets
                
                counted = 
                    Dict.empty
                
                total =
                    coordinateAndDistanceTargets
                    |> Array.foldl (\tuples totalSet ->
                        let
                            closestTarget = Tuple.second (Tuple.second tuples)
                        in
                        updateCountedCoordinateDictionary closestTarget.label totalSet) counted
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                -- totallog = log "total" total

                tiles =
                    coordinateAndDistanceTargets
                    |> Array.foldl (\tuples acc ->
                        let
                            coordinate = Tuple.first tuples
                            target = Tuple.second (Tuple.second tuples)
                        in
                         List.append [{coordinate | label = target.label}] acc) []
                -- tileslog = log "tiles" tiles
            in
            { model | tiles = tiles }
            
        ParseCoordinatesText ->
            let
                coordinates =
                    parseCoordinates model.coordinatesText
                -- coordinatesLog =log "coordinates" coordinates

                -- coord1 = Coordinate (Point 233 472) (PointFloat (toFloat 233) (toFloat 472)) "a"
                -- coord2 = Coordinate (Point 124 562) (PointFloat (toFloat 124) (toFloat 562)) "b"
                -- dist1 = getDistanceBetweenCoordinates coord1 coord2
                -- log1 = log "dist1" dist1 // 19, now 197, w00t w00000t
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
    |> Canvas.fillStyle (Color.rgb 0 0 0)
    -- |> Canvas.fillCircle coordinate.pointFloat.x coordinate.pointFloat.y 4
    |> Canvas.fillRect coordinate.pointFloat.x (coordinate.pointFloat.y - 12) 20 14
    |> Canvas.fillStyle canvasCoordinateDotColor
    |> Canvas.font "14px Helvetica"
    |> Canvas.fillText coordinate.label coordinate.pointFloat.x coordinate.pointFloat.y Nothing

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
    -- |> drawPlot tile (Color.rgb 255 255 255)

drawTiles : List Coordinate -> Dict String Color -> Commands -> Commands
drawTiles tiles labelColors cmds =
        List.foldl (\tile acc -> drawTile tile labelColors acc) cmds tiles

 
--  drawCoordinate : Coordinate -> Commands -> Commands
-- drawCoordinate coordinate cmds =
--     cmds
--     |> Canvas.fillStyle (Color.rgb 0 0 0)
--     -- |> Canvas.fillCircle coordinate.pointFloat.x coordinate.pointFloat.y 4
--     |> Canvas.fillRect coordinate.pointFloat.x (coordinate.pointFloat.y - 12) 20 14
--     |> Canvas.fillStyle canvasCoordinateDotColor
--     |> Canvas.font "14px Helvetica"
--     |> Canvas.fillText coordinate.label coordinate.pointFloat.x coordinate.pointFloat.y Nothing

drawLegend : Dict String Color -> Int -> Int -> Coordinate -> Commands -> Commands
drawLegend labelColors x y coordinate cmds =
    let
        color = (getColorElseWhite coordinate.label labelColors)
        xFloat = toFloat x
        yFloat = toFloat y
    in
    cmds
    |> Canvas.fillStyle color
    |> Canvas.fillRect xFloat yFloat 20 11
    |> Canvas.fillStyle (Color.rgb 255 255 255)
    |> Canvas.font "10px Helvetica"
    |> Canvas.fillText coordinate.label xFloat (yFloat + 10) Nothing

-- (\cmds -> List.foldl (drawLegend model.labelColors) cmds model.coordinates)
drawLegendFromCoordinates labelColors startX startY coordinates cmds =
    List.indexedMap (\index coord -> (startY + (12 * index), coord)) coordinates
    |> List.foldl (\tuple acc ->
        let
            y = Tuple.first tuple
            coord = Tuple.second tuple
        in
        acc |> drawLegend labelColors startX y coord) cmds
        

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
                            [ style "border" "1px solid black"
                                -- , style "width" "600px"
                                ]
                            ( Canvas.empty
                                |> Canvas.clearRect 0 0 getColsFloat getRowsFloat
                                |> renderBackground getColsFloat getRowsFloat
                                |> (\cmds -> drawTiles model.tiles model.labelColors cmds)
                                |> (\cmds -> List.foldl drawCoordinate cmds model.coordinates)
                                |> (drawLegendFromCoordinates model.labelColors 580 0 model.coordinates)
                                -- |> (\cmds -> List.foldl (drawLegend model.labelColors) cmds model.coordinates)
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