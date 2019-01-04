{- 

    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden

    https://adventofcode.com/2018/day/7
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
    { stepsText : String
    , steps : List Step
    }

initialModel : Model
initialModel =
    { stepsText = ""
    , steps = []
    }

type Msg
    = InputStepsText String -- when you type or paste in the text area
    | ParseStepsText -- RUN THE MAGIC
    | LoadFromCache -- loads strings vs. you copy pasta

-- [Challenge 1 Functions] ------------------------------------------------------------

type alias Step = {
    label: String
    , before : Maybe String }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseStepsText ->
            let
                -- ** Challenge 1 **
                log1 = log "cow" "cow"
            in
            model

        InputStepsText text ->
            { model | stepsText = text }

        LoadFromCache ->
            { model | stepsText = stepsCache }


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
                                        , onInput InputStepsText
                                        ]
                                        [ Html.text model.stepsText ]
                                    ]
                                ]
                            ]
                        , div [class "mdl-cell mdl-cell--6-col"][
                            
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
                    , onClick ParseStepsText
                    ]
                    [ Html.text "2. Parse Steps" ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    -- , onClick CalculateCoordinateDistance
                    ]
                    [ Html.text "3. Calculate Step Order" ]

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

stepsCache : String
stepsCache =
    """Step G must be finished before step N can begin.
Step N must be finished before step B can begin.
Step P must be finished before step Q can begin.
Step F must be finished before step U can begin.
Step H must be finished before step A can begin.
Step C must be finished before step S can begin.
Step A must be finished before step K can begin.
Step M must be finished before step O can begin.
Step V must be finished before step L can begin.
Step E must be finished before step L can begin.
Step B must be finished before step Q can begin.
Step W must be finished before step J can begin.
Step R must be finished before step D can begin.
Step D must be finished before step S can begin.
Step S must be finished before step X can begin.
Step Q must be finished before step J can begin.
Step I must be finished before step L can begin.
Step U must be finished before step J can begin.
Step Z must be finished before step X can begin.
Step Y must be finished before step T can begin.
Step J must be finished before step K can begin.
Step T must be finished before step L can begin.
Step K must be finished before step O can begin.
Step O must be finished before step X can begin.
Step L must be finished before step X can begin.
Step Y must be finished before step O can begin.
Step F must be finished before step S can begin.
Step K must be finished before step L can begin.
Step Z must be finished before step O can begin.
Step J must be finished before step X can begin.
Step K must be finished before step X can begin.
Step Q must be finished before step X can begin.
Step Y must be finished before step L can begin.
Step E must be finished before step S can begin.
Step H must be finished before step Y can begin.
Step G must be finished before step P can begin.
Step E must be finished before step K can begin.
Step B must be finished before step L can begin.
Step T must be finished before step K can begin.
Step N must be finished before step R can begin.
Step F must be finished before step E can begin.
Step W must be finished before step Y can begin.
Step U must be finished before step X can begin.
Step A must be finished before step I can begin.
Step Q must be finished before step Y can begin.
Step P must be finished before step T can begin.
Step D must be finished before step X can begin.
Step E must be finished before step Y can begin.
Step F must be finished before step B can begin.
Step P must be finished before step I can begin.
Step N must be finished before step S can begin.
Step F must be finished before step V can begin.
Step W must be finished before step U can begin.
Step F must be finished before step A can begin.
Step I must be finished before step Z can begin.
Step E must be finished before step D can begin.
Step R must be finished before step I can begin.
Step M must be finished before step V can begin.
Step R must be finished before step U can begin.
Step R must be finished before step X can begin.
Step G must be finished before step O can begin.
Step G must be finished before step H can begin.
Step M must be finished before step R can begin.
Step E must be finished before step U can begin.
Step F must be finished before step Z can begin.
Step N must be finished before step Q can begin.
Step U must be finished before step O can begin.
Step J must be finished before step T can begin.
Step W must be finished before step Z can begin.
Step I must be finished before step J can begin.
Step U must be finished before step L can begin.
Step I must be finished before step X can begin.
Step Z must be finished before step J can begin.
Step F must be finished before step D can begin.
Step N must be finished before step O can begin.
Step Q must be finished before step U can begin.
Step G must be finished before step L can begin.
Step H must be finished before step Q can begin.
Step M must be finished before step Q can begin.
Step N must be finished before step D can begin.
Step Z must be finished before step L can begin.
Step I must be finished before step Y can begin.
Step E must be finished before step X can begin.
Step J must be finished before step L can begin.
Step H must be finished before step W can begin.
Step P must be finished before step Y can begin.
Step Q must be finished before step T can begin.
Step Z must be finished before step Y can begin.
Step R must be finished before step T can begin.
Step E must be finished before step J can begin.
Step I must be finished before step T can begin.
Step A must be finished before step L can begin.
Step E must be finished before step R can begin.
Step T must be finished before step O can begin.
Step Y must be finished before step X can begin.
Step A must be finished before step Q can begin.
Step W must be finished before step Q can begin.
Step A must be finished before step T can begin.
Step B must be finished before step Y can begin.
Step H must be finished before step E can begin.
Step H must be finished before step K can begin."""