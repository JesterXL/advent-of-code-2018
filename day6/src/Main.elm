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
    , coordinates : List Point
    , coordinateFloats : List PointFloat
    }

initialModel : Model
initialModel =
    { coordinatesText = ""
    , coordinates = []
    , coordinateFloats = []
    }

type Msg
    = InputCoordinatesText String -- when you type or paste in the text area
    | ParseCoordinatesText -- RUN THE MAGIC
    | LoadFromCache -- loads strings vs. you copy pasta

-- [Challenge 1 Functions] ------------------------------------------------------------

type alias Point =
    { x : Int   
    , y : Int }

type alias PointFloat =
    { x : Float
    , y: Float }

intListToPoint : List Int -> Point
intListToPoint intList =
    let
        xAndYList = Array.fromList intList
        x = Array.get 0 xAndYList |> Maybe.withDefault 0
        y = Array.get 1 xAndYList |> Maybe.withDefault 0
    in
    Point x y

-- parseCoordinates : String -> List (List (Int, Int))
parseCoordinates string =
    String.split "\n" string
    |> List.map (String.split ",")
    |> List.map (List.map String.trim)
    |> List.map (List.map String.toInt)
    |> List.map (List.map (Maybe.withDefault 0))
    |> List.map intListToPoint

-- getManhattanDistance =

-- function getToroidManhattanDistance( node1, node2, size )
-- {
--     var dx = Math.min( Math.abs( node1.x - node2.x ), size - Math.abs( node2.x - node1.x ) );
--     var dy = Math.min( Math.abs( node1.y - node2.y ), size - Math.abs( node2.y - node1.y ) );
--     return dx + dy;    
-- }


-- module.exports = function distance(a, b) {
--   var distance = 0
--   var dimensions = Math.max(a.length, b.length)
--   for (var i = 0; i < dimensions; i++) {
--     distance += Math.abs((b[i] || 0) - (a[i] || 0))
--   }
--   return distance
-- }

update : Msg -> Model -> Model
update msg model =
    case msg of
        -- do the magic
        ParseCoordinatesText ->
            let
                -- ** Challenge 1 **
                coordinates =
                    parseCoordinates model.coordinatesText
                coordinatesLog = log "coordinates" coordinates
                coordinateFloats =
                    coordinatesToFloats coordinates

            in
            { model | coordinates = coordinates, coordinateFloats = coordinateFloats }

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
    500

getCols : Int
getCols =
    500

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

drawCoordinate point cmds =
    cmds
    |> Canvas.fillStyle canvasCoordinateDotColor
    |> Canvas.fillCircle point.x point.y 4

coordinatesToFloats : List Point -> List PointFloat
coordinatesToFloats coordinates =
    List.map (\point -> PointFloat (toFloat point.x) (toFloat point.y)) coordinates


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
                        
                        -- , div [ class "mdl-card__supporting-text" ][
                        --         div [ class "textarea_label" ] [ text "Starting Units Count:"]
                        --         , text (String.fromInt (String.length model.coordinatesText))
                        --     ]
                        -- , div [ class "mdl-card__supporting-text" ]
                        --     [ div [ class "textarea_label" ] [ text "Remaining Units:"]
                        --     , textarea
                        --         [ class "mdl-textfield__input"
                        --         , rows 2
                        --         , required False
                        --         ]
                        --         [ text model.remainingUnits ]
                        --     ]
                        --     , div [ class "mdl-card__supporting-text" ][
                        --         div [ class "textarea_label" ] [ text "Remaining Units Count:"]
                        --         , text (String.fromInt (String.length model.remainingUnits))
                        --     , div [ class "textarea_label" ] [ text "Best Individual Reduce Count:"]
                        --     , text model.smallestUnit
                        --     ]
                    ]
                    , div [][
                        Canvas.element
                            getCols
                            getRows
                            [ style "border" "1px solid black", style "width" "300px"]
                            ( Canvas.empty
                                |> Canvas.clearRect 0 0 getColsFloat getRowsFloat
                                |> renderBackground getColsFloat getRowsFloat
                                |> (\cmds -> List.foldl drawCoordinate cmds model.coordinateFloats)
                                |> Canvas.fillText "Hello world" 50 100 Nothing
                                -- , Canvas.text
                                --     [ Canvas.align Canvas.Right
                                --     , Canvas.font { size = 30, family = "sans-serif" }
                                --     , Canvas.lineWidth 1
                                --     , Canvas.stroke Color.blue
                                --     , Canvas.fill Color.green
                                --     ]
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