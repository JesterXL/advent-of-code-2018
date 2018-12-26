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
import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, int, spaces, oneOf, keyword)

type alias Model =
    { sleepTimesText : String
    , sleepTimes : List SleepTime
    , guardSleepSchedule : Dict Int GuardSleepTime
    , challenge1 : Int
    }

initialModel : Model
initialModel =
    { sleepTimesText = ""
    , sleepTimes = []
    , guardSleepSchedule = Dict.empty
    , challenge1 = 0
    }

type Msg
    = InputSleepTimesText String -- when you type or paste in the text area
    | ParseSleepTimesText -- RUN THE MAGIC
    | LoadFromCache -- loads strings vs. you copy pasta
    | CalculateGuardSleepSchedule


-- [Challenge 1 Functions] ------------------------------------------------------------

type GuardStatus =
    StartShift Int
    | Sleep
    | Wake

type alias DateTime =
 {  year : Int
    , month : Int
    , date : Int
    , hour : Int
    , minute : Int }

defaultDateTime : DateTime
defaultDateTime =
    DateTime 0 0 0 0 0

type alias SleepTime = 
    { dateTime : DateTime
    , status : GuardStatus
    }

type alias GuardSleepTime =
    { id : Int
    , start: DateTime
    , sleep : List DateTime
    , wake : List DateTime
    , totalMinutes : Int
    , maxMinute : (Int, Int)  }

getGuardSleepTime : Int -> DateTime -> List DateTime -> List DateTime -> GuardSleepTime
getGuardSleepTime id start sleep wake =
    let
        base = GuardSleepTime id start sleep wake 0 (0, 0)
    in
        { base | totalMinutes = (sumSleepAndWake base)}

-- getSleepTime : Int -> Int -> Int -> Int -> Int -> GuardStatus -> SleepTime
getSleepTime year month date hour minute status =
    SleepTime (DateTime year month date hour minute) status

parseSleepTimes string =
    split "\n" string
    |> List.map (Parser.run parseSleepTimeString)
    |> List.map (Result.withDefault (getSleepTime 0 0 0 0 0 (StartShift 0) ))


parseLeadingZero : Parser Int
parseLeadingZero =
    oneOf
        [ succeed identity
            |. symbol "0"
            |= int
        , int
        ]

parseSleepTimeString : Parser SleepTime
parseSleepTimeString =
    succeed getSleepTime
        |. symbol "["
        |= int
        |. symbol "-"
        |= parseLeadingZero
        |. symbol "-"
        |= parseLeadingZero
        |. symbol " "
        |= parseLeadingZero
        |. symbol ":"
        |= parseLeadingZero
        |. symbol "] "
        |= parseGuardStatus

parseGuardStatus : Parser GuardStatus
parseGuardStatus =
    oneOf
        [ succeed StartShift
            |. symbol "Guard #"
            |= int
            |. symbol " begins shift"
        , succeed Sleep
            |. keyword "falls asleep"
        , succeed Wake
            |. keyword "wakes up"]

compareDateTimeYear : DateTime -> DateTime -> Order
compareDateTimeYear a b =
    compare a.year b.year

compareDateTimeMonth : DateTime -> DateTime -> Order
compareDateTimeMonth a b =
    compare a.month b.month

compareDateTimeDate : DateTime -> DateTime -> Order
compareDateTimeDate a b =
    compare a.date b.date

compareDateTimeHour : DateTime -> DateTime -> Order
compareDateTimeHour a b =
    compare a.hour b.hour

compareDateTimeMinute : DateTime -> DateTime -> Order
compareDateTimeMinute a b =
    compare a.minute b.minute

compareDateTimes : DateTime -> DateTime -> Order
compareDateTimes a b =
    case compareDateTimeMonth a b of
            EQ ->
                case compareDateTimeDate a b of
                    EQ ->
                        case compareDateTimeHour a b of
                            EQ ->
                                compareDateTimeMinute a b
                            _ ->
                                compareDateTimeHour a b
                    _ ->
                        compareDateTimeDate a b
            _ ->
                compareDateTimeMonth a b

compareSleepTimes : SleepTime -> SleepTime -> Order
compareSleepTimes sleepTimeA sleepTimeB =
    compareDateTimes sleepTimeA.dateTime sleepTimeB.dateTime

compareGuardSleepTimes : GuardSleepTime -> GuardSleepTime -> Order
compareGuardSleepTimes a b =
    compareDateTimes a.start b.start

updateScheduleDict : Dict Int (List SleepTime) -> Int -> SleepTime -> Dict Int (List SleepTime)
updateScheduleDict dict id sleepTime =
    Dict.update id (\sleepTimesMaybe ->
        case sleepTimesMaybe of
            Nothing ->
                Just [sleepTime]
            Just val ->
                Just (List.append [sleepTime] val)) dict

-- type alias GuardSleepTime =
    -- { id : Int
    -- , start: DateTime
    -- , sleep : List DateTime
    -- , wake : List DateTime }
    
updateGuardSleepSchedule : Dict Int GuardSleepTime -> Int -> SleepTime -> Dict Int GuardSleepTime
updateGuardSleepSchedule dict id sleepTime =
    Dict.update id (\sleepTimesMaybe ->
        case sleepTimesMaybe of
            Nothing ->
                case sleepTime.status of
                    StartShift guardID ->
                        Just (getGuardSleepTime id sleepTime.dateTime [] [])
                    Sleep ->
                        Just (getGuardSleepTime id defaultDateTime [sleepTime.dateTime] [])
                    Wake ->
                        Just (getGuardSleepTime id defaultDateTime [] [sleepTime.dateTime])
            Just guardSleepTime ->
                -- Just (List.append [sleepTime] val)) dict
                case sleepTime.status of
                    StartShift guardID ->
                        Just { guardSleepTime | start = sleepTime.dateTime }
                    Sleep ->
                        Just { guardSleepTime | sleep = List.append [sleepTime.dateTime] guardSleepTime.sleep }
                    Wake ->
                        Just { guardSleepTime | wake = List.append [sleepTime.dateTime] guardSleepTime.wake }) dict


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- do the magic
        ParseSleepTimesText ->
            let
                sleepTimes =
                    parseSleepTimes sleepTimesCacheString
                    |> List.sortWith compareSleepTimes
            in
            { model | sleepTimes = sleepTimes }
        
        CalculateGuardSleepSchedule ->
            let
                -- guardSleepScheduleDict =
                --     List.foldl (\sleepTime acc ->
                --         case sleepTime.status of
                --             StartShift guardID ->
                --                 { acc | id = guardID, parts = updateScheduleDict acc.parts guardID sleepTime }
                --             Sleep ->
                --                 { acc | parts = updateScheduleDict acc.parts acc.id sleepTime }
                --             Wake ->
                --                 { acc | parts = updateScheduleDict acc.parts acc.id sleepTime }
                --         ) {id = 0, parts = Dict.empty} model.sleepTimes
                --         |> .parts
                --         -- |> Dict.foldl (\guardID parts acc -> ) Dict.empty 
                -- guardSleepScheduleDictLog = log "guardSleepScheduleDict" guardSleepScheduleDict

                -- type alias GuardSleepTime =
                -- { id : Int
                -- , start: DateTime
                -- , sleep : List DateTime
                -- , wake : List DateTime }

                -- ** Challenge 1 **
                -- Attempt #1: 1049, not the right answer, answer too low (id of the guard with most minutes)
                -- ... whoa, I didn't even read the question, lol, ok, multipled by start minute, my bad
                -- Attempt #2: 17833, too low
                -- Attempt #3: 38813. OH YEAH, who's da man!?


                guardSleepScheduleDict =
                    List.foldl (\sleepTime acc ->
                        case sleepTime.status of
                            StartShift guardID ->
                                { acc | id = guardID, parts = updateGuardSleepSchedule acc.parts guardID sleepTime }
                            Sleep ->
                                { acc | parts = updateGuardSleepSchedule acc.parts acc.id sleepTime }
                            Wake ->
                                { acc | parts = updateGuardSleepSchedule acc.parts acc.id sleepTime }
                        ) {id = 0, parts = Dict.empty} model.sleepTimes
                        |> .parts
                        |> Dict.map (\k v -> { v | totalMinutes = sumSleepAndWake v})
                        |> Dict.map (\k v -> { v | maxMinute = foldMinuteValues v})
                -- guardSleepScheduleDictLog = log "guardSleepScheduleDict" guardSleepScheduleDict
                biggestSleeper =
                    Dict.values guardSleepScheduleDict 
                    |> List.sortBy .totalMinutes 
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault (getGuardSleepTime 0 (DateTime 0 0 0 0 0) [] [])

                biggestSleeperLog = log "biggestSleeper" biggestSleeper
                challenge1 = biggestSleeper.id * (Tuple.first biggestSleeper.maxMinute)
                challenge1Log = log "challenge1" challenge1

            in
            { model | guardSleepSchedule = guardSleepScheduleDict, challenge1 = challenge1}
            -- model

        -- when you type or copy pasta into the text area
        InputSleepTimesText text ->
            { model | sleepTimesText = text }

        -- put the sleep strings function text into the text area
        LoadFromCache ->
            { model | sleepTimesText = sleepTimesCacheString }

getStatusString : GuardStatus -> String
getStatusString guardStatus =
    case guardStatus of
        Sleep ->
            "Sleep"
        Wake ->
            "Wakes"
        StartShift id ->
            (String.fromInt id) ++ " begins shift"

getStatusIcon : GuardStatus -> String
getStatusIcon guardStatus =
    case guardStatus of
        Sleep ->
            "hotel"
        Wake ->
            "local_cafe"
        StartShift id ->
            "person"

sleepTimeListItem : SleepTime -> Html Msg
sleepTimeListItem sleepTime =
    li [ class "mdl-list__item mdl-list__item--two-line"][
        span [ class "mdl-list__item-primary-content"][
            i [ class "material-icons mdl-list__item-icon"][text (getStatusIcon sleepTime.status)]
            , span [][text (getStatusString sleepTime.status)]
            , span [ class "mdl-list__item-sub-title"][ text (formatSleepTime sleepTime)]
        ]
    ]

formatLeadingZero : Int -> String
formatLeadingZero num =
    let
        val = String.fromInt num
    in
    if String.length val < 2 then
        "0" ++ val
    else
        val

formatSleepTime : SleepTime -> String
formatSleepTime sleepTime =
   formatLeadingZero sleepTime.dateTime.hour ++ ":" ++ formatLeadingZero sleepTime.dateTime.minute ++ " " ++ formatLeadingZero sleepTime.dateTime.date ++ "/" ++ formatLeadingZero sleepTime.dateTime.month ++ "/" ++ formatLeadingZero sleepTime.dateTime.year


formatSleepTimeMonthDate : GuardSleepTime -> String
formatSleepTimeMonthDate guardSleepTime =
    formatLeadingZero guardSleepTime.start.date ++ "-" ++ formatLeadingZero guardSleepTime.start.month

-- minuteToMilliseconds : Int -> Int
-- minuteToMilliseconds minute =
--     minute * 60 * 1000

dateDifference : DateTime -> DateTime -> Int
dateDifference a b =
    case compareDateTimes a b of
        LT ->
            b.minute - a.minute
        GT ->
            a.minute - b.minute
        EQ ->
            0

getStartMinute : DateTime -> DateTime -> Int
getStartMinute a b =
    case compareDateTimes a b of
        LT ->
            a.minute
        GT ->
            b.minute
        EQ ->
            a.minute

sumSleepAndWake : GuardSleepTime -> Int
sumSleepAndWake guardSleepTime =
    let
        sleep = Array.fromList guardSleepTime.sleep
        wake = Array.fromList guardSleepTime.wake
        -- log0 = log "start --" "start"
        total =
            Array.indexedMap (\index value ->
                let
                    sleepTime = value
                    -- [jwarden 12.25.2018] Note, this could realllly screw up the math, lol, but... uh.... YOLO
                    wakeTime = Array.get index wake |> Maybe.withDefault defaultDateTime
                    -- log1 = log "sleepTime" sleepTime
                    -- log2 = log "wakeTime" wakeTime
                    diff = dateDifference sleepTime wakeTime
                    -- log3 = log "diff" diff
                    -- { start = getStartMinute sleepTime wakeTime , total = diff}
                in
                    diff) sleep
            |> Array.toList
            |> List.sum
    in
        total


sum2Arrays : Array Int -> Array Int -> Array Int
sum2Arrays array1 array2 =
    Array.indexedMap (\index value->
        let
            value2 =
                Array.get index array2
                |> Maybe.withDefault 0
        in
        value2 + value) array1

foldMinuteValues guardSleepTime =
    let
        sleep = Array.fromList guardSleepTime.sleep
        wake = Array.fromList guardSleepTime.wake
        minuteList =
            Array.indexedMap (\index value ->
                let
                    sleepTime = value
                    wakeTime = Array.get index wake |> Maybe.withDefault defaultDateTime
                    total = dateDifference sleepTime wakeTime
                    start = getStartMinute sleepTime wakeTime
                    end = start + total
                    final =
                        Array.initialize 60 (always 0)
                        |> Array.indexedMap (\minuteIndex minuteValue ->
                            if minuteIndex >= start && minuteIndex < end then
                                minuteValue + 1
                            else
                                minuteValue)
                in
                    final) sleep
            |> Array.foldl (\valueList acc -> sum2Arrays valueList acc) (Array.initialize 60 (always 0))
            |> Array.toIndexedList 
            |> List.sortBy Tuple.second
            |> List.reverse
            |> List.head
            |> Maybe.withDefault (0, 0)
        -- minuteListLog = log "minuteList" minuteList
    in
        minuteList


-- buildProgressBar : GuardSleepTime -> Html Msg
-- buildProgressBar guardSleepTime =
--     let
--         total = sumSleepAndWake guardSleepTime
--         totallog = log "total" total
--     in
--     div[ style "border" "1px solid #ccc"
--         , style "display" "block"][
--         div [style "background-color" "#9e9e9e" 
--             , style "height" "24px"
--             , style "width" "35%"
--             , style "padding" "0.01em 16px"][]
--     ]

buildTableRow : GuardSleepTime -> Html Msg
buildTableRow guardSleepTime =
    tr[][
        td [class "mdl-data-table__cell--non-numeric"][ text (formatSleepTimeMonthDate guardSleepTime)]
        , td [][text ("#" ++ String.fromInt guardSleepTime.id)]
        , td [][ text (String.fromInt guardSleepTime.totalMinutes) ]
        , td [][ text (String.fromInt (Tuple.second guardSleepTime.maxMinute)) ]
    ]

buildTable : Dict Int GuardSleepTime -> Html Msg
buildTable guardSleepTimes =
    table [class "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" , style "width" "100%"][
        thead[][
            tr[][
                th[class "mdl-data-table__cell--non-numeric"][text "Date"] 
                , th[][text "ID"] 
                , th[][text "Total Sleeping Minutes"]
                , th[][text "Most Slept Minute"]
            ]
        ]
        , tbody[] (Dict.values guardSleepTimes |> List.sortBy .totalMinutes |> List.reverse |> List.map buildTableRow)
    ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "demo-card-wide mdl-card mdl-shadow--2dp" ]
            [ div [ class "mdl-card__title" ]
                [ h2 [ class "mdl-card__title-text" ] [ text "Claims Parser" ]
                ]
            , div [ class "mdl-card__supporting-text" ] [ text "1. click 'Load Cached'", br [] [], text "2. click 'Parse Claims'" ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "mdl-textfield__input"
                        , rows 3
                        , placeholder "Paste claims text here"
                        , required True
                        , onInput InputSleepTimesText
                        ]
                        [ text model.sleepTimesText ]
                    ]
                    , div[class "mdl-grid"][
                        if List.length model.sleepTimes > 0 then
                            div [ class "mdl-cell mdl-cell--4-col" ][
                                p[][text "Sleep Times:"]
                                , ul[ class "event-list mdl-list"]
                                (List.map sleepTimeListItem model.sleepTimes)
                            ]
                        else
                            div [ class "mdl-cell mdl-cell--4-col" ][
                               div [ style "height" "302px"][]
                            ]
                        , if Dict.size model.guardSleepSchedule > 0 then
                            div [ class "mdl-cell mdl-cell--4-col" ][
                                    p[][text "Sleep Schedule:"]
                                    , buildTable model.guardSleepSchedule
                                ]
                        else
                            div [ class "mdl-cell mdl-cell--4-col" ][
                               div [ style "height" "302px"][]
                            ]
                        , div [class "mdl-cell mdl-cell--4-col"][]
                    ]

                -- , div [ class "mdl-card__supporting-text" ]
                --     [ div [ class "textarea_label" ] [ text "Square Inches:" ]
                --     , text <| String.fromInt model.squareInches
                --     , div [ class "textarea_label" ] [ text "No Overlap Claim ID:" ]
                --     , text <| String.fromInt model.noOverlapClaimID
                --     ]
                -- , div [][
                --         Canvas.element
                --             1000
                --             1000
                --             [ style "border" "1px solid black", style "width" "300px"]
                --             ( Canvas.empty
                --                 |> Canvas.clearRect 0 0 1000 1000
                --                 |> renderBackground
                --                 |> (\cmds -> Array.foldl renderClaim cmds model.claims)
                --                 |> (\cmds -> Array.foldl renderOverlap cmds model.overlappedRectangles)
                --                 |> (\cmds -> renderNoOverlap model.noOverlapRectangle cmds)
                --                 )
                --     ]
                ]
            , div [ class "mdl-card__actions mdl-card--border" ]
                [ a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick LoadFromCache
                    ]
                    [ text "1. Load Cached" ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick ParseSleepTimesText
                    ]
                    [ text "2. Parse Sleep Times" ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick CalculateGuardSleepSchedule
                    ]
                    [ text "3. Calculate Sleep Schedule" ]
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

sleepTimesCacheString : String
sleepTimesCacheString =
    """[1518-05-24 23:56] Guard #1721 begins shift
[1518-08-22 00:09] falls asleep
[1518-05-19 00:53] wakes up
[1518-04-19 23:48] Guard #773 begins shift
[1518-09-24 00:35] wakes up
[1518-07-15 00:56] wakes up
[1518-08-10 00:03] Guard #769 begins shift
[1518-07-10 00:50] wakes up
[1518-07-12 00:54] falls asleep
[1518-05-30 00:51] wakes up
[1518-09-15 00:56] wakes up
[1518-09-14 00:15] falls asleep
[1518-03-30 00:57] falls asleep
[1518-10-19 00:55] wakes up
[1518-09-18 00:58] wakes up
[1518-11-18 00:38] wakes up
[1518-11-07 00:23] falls asleep
[1518-10-18 00:56] wakes up
[1518-03-05 00:41] falls asleep
[1518-05-19 00:36] falls asleep
[1518-09-04 00:49] wakes up
[1518-06-08 00:56] falls asleep
[1518-06-11 23:56] Guard #2749 begins shift
[1518-09-27 00:54] wakes up
[1518-10-03 00:49] wakes up
[1518-09-16 00:45] wakes up
[1518-10-06 00:46] wakes up
[1518-07-20 00:07] falls asleep
[1518-05-09 00:55] wakes up
[1518-05-11 00:05] falls asleep
[1518-10-14 00:45] falls asleep
[1518-08-21 00:19] wakes up
[1518-11-01 00:43] wakes up
[1518-08-31 00:57] wakes up
[1518-03-28 00:58] wakes up
[1518-04-06 00:12] wakes up
[1518-02-28 00:03] Guard #191 begins shift
[1518-09-29 00:01] falls asleep
[1518-05-15 00:47] falls asleep
[1518-03-04 00:01] Guard #2879 begins shift
[1518-04-28 00:29] wakes up
[1518-05-10 00:40] falls asleep
[1518-03-12 00:03] Guard #191 begins shift
[1518-09-19 00:41] falls asleep
[1518-07-28 00:00] Guard #1381 begins shift
[1518-04-11 00:01] Guard #1907 begins shift
[1518-04-02 00:58] wakes up
[1518-03-04 00:51] wakes up
[1518-05-02 00:22] falls asleep
[1518-07-01 00:02] falls asleep
[1518-07-21 00:24] falls asleep
[1518-05-31 00:31] wakes up
[1518-08-02 00:00] Guard #2713 begins shift
[1518-08-25 00:48] falls asleep
[1518-05-02 00:26] wakes up
[1518-07-08 00:00] Guard #2711 begins shift
[1518-09-16 00:38] falls asleep
[1518-07-22 00:43] wakes up
[1518-11-04 00:13] falls asleep
[1518-09-03 23:50] Guard #97 begins shift
[1518-03-20 00:35] wakes up
[1518-05-17 00:41] wakes up
[1518-08-22 00:32] falls asleep
[1518-05-07 23:58] Guard #773 begins shift
[1518-08-28 00:12] wakes up
[1518-09-06 00:46] falls asleep
[1518-05-20 00:59] wakes up
[1518-03-19 00:59] wakes up
[1518-08-22 00:29] wakes up
[1518-07-06 00:00] Guard #2777 begins shift
[1518-06-15 00:37] falls asleep
[1518-03-15 00:27] falls asleep
[1518-09-10 00:43] wakes up
[1518-09-30 00:49] falls asleep
[1518-09-14 23:58] Guard #1493 begins shift
[1518-06-20 00:33] falls asleep
[1518-05-04 00:53] falls asleep
[1518-09-04 00:45] falls asleep
[1518-07-17 00:56] wakes up
[1518-06-18 00:58] wakes up
[1518-04-07 00:42] falls asleep
[1518-05-23 23:58] Guard #2777 begins shift
[1518-10-03 23:57] Guard #1721 begins shift
[1518-03-04 23:57] Guard #773 begins shift
[1518-05-31 00:20] wakes up
[1518-08-04 00:04] Guard #1213 begins shift
[1518-05-11 00:56] wakes up
[1518-03-02 00:32] wakes up
[1518-10-17 00:50] wakes up
[1518-11-08 00:39] falls asleep
[1518-09-03 00:20] falls asleep
[1518-10-28 00:40] falls asleep
[1518-08-20 00:51] falls asleep
[1518-08-15 00:36] wakes up
[1518-06-08 00:51] wakes up
[1518-08-20 00:58] wakes up
[1518-03-30 00:00] Guard #2711 begins shift
[1518-10-31 00:00] falls asleep
[1518-06-24 23:59] Guard #97 begins shift
[1518-02-21 00:00] Guard #2749 begins shift
[1518-06-04 00:02] Guard #1907 begins shift
[1518-06-28 23:51] Guard #1493 begins shift
[1518-02-18 23:59] Guard #2141 begins shift
[1518-04-17 00:17] wakes up
[1518-06-15 00:57] wakes up
[1518-04-02 00:27] falls asleep
[1518-06-08 00:10] falls asleep
[1518-10-05 00:41] wakes up
[1518-11-15 00:51] falls asleep
[1518-05-17 23:59] Guard #991 begins shift
[1518-11-22 00:53] wakes up
[1518-07-24 00:10] falls asleep
[1518-06-25 00:27] falls asleep
[1518-02-20 00:46] falls asleep
[1518-09-29 00:34] falls asleep
[1518-10-03 00:01] Guard #769 begins shift
[1518-06-22 00:05] falls asleep
[1518-02-26 00:49] falls asleep
[1518-03-13 00:24] falls asleep
[1518-05-03 00:38] wakes up
[1518-07-08 00:22] falls asleep
[1518-03-14 00:08] falls asleep
[1518-07-20 00:49] wakes up
[1518-03-13 00:58] wakes up
[1518-09-30 00:51] wakes up
[1518-10-10 00:39] falls asleep
[1518-09-05 00:02] Guard #1049 begins shift
[1518-10-26 00:49] falls asleep
[1518-07-22 00:33] falls asleep
[1518-09-27 00:13] falls asleep
[1518-11-09 00:48] falls asleep
[1518-11-16 00:11] falls asleep
[1518-05-04 00:50] wakes up
[1518-06-14 00:02] Guard #2879 begins shift
[1518-03-06 00:44] wakes up
[1518-07-23 00:47] wakes up
[1518-04-16 00:59] wakes up
[1518-09-23 00:05] falls asleep
[1518-09-12 00:27] wakes up
[1518-02-21 00:49] wakes up
[1518-07-05 00:58] wakes up
[1518-07-22 00:28] wakes up
[1518-07-14 00:03] Guard #2917 begins shift
[1518-05-14 00:02] Guard #1277 begins shift
[1518-04-06 00:53] wakes up
[1518-05-04 00:59] wakes up
[1518-09-07 00:50] wakes up
[1518-04-03 00:26] falls asleep
[1518-10-09 00:43] falls asleep
[1518-07-10 00:41] falls asleep
[1518-04-10 00:49] falls asleep
[1518-03-14 00:47] wakes up
[1518-05-03 00:54] wakes up
[1518-04-15 00:39] wakes up
[1518-07-31 00:40] wakes up
[1518-08-01 00:02] falls asleep
[1518-10-06 23:57] Guard #191 begins shift
[1518-11-04 00:23] wakes up
[1518-08-15 00:49] falls asleep
[1518-02-28 00:29] falls asleep
[1518-03-30 23:59] Guard #1907 begins shift
[1518-08-01 00:40] falls asleep
[1518-10-13 00:52] wakes up
[1518-10-14 00:01] Guard #2917 begins shift
[1518-08-26 00:00] Guard #2917 begins shift
[1518-07-24 00:02] Guard #2749 begins shift
[1518-04-21 00:25] falls asleep
[1518-03-05 00:43] wakes up
[1518-08-31 23:58] Guard #1721 begins shift
[1518-08-11 00:28] falls asleep
[1518-08-26 00:41] wakes up
[1518-05-18 00:20] falls asleep
[1518-08-09 00:03] Guard #683 begins shift
[1518-08-24 00:16] wakes up
[1518-03-26 00:27] wakes up
[1518-06-19 00:35] wakes up
[1518-09-24 00:56] wakes up
[1518-11-01 00:34] falls asleep
[1518-10-11 00:13] falls asleep
[1518-10-27 00:18] falls asleep
[1518-09-03 00:53] wakes up
[1518-04-10 00:33] wakes up
[1518-07-30 00:57] wakes up
[1518-06-10 00:12] falls asleep
[1518-03-31 00:31] falls asleep
[1518-11-21 23:57] Guard #2141 begins shift
[1518-03-21 00:31] falls asleep
[1518-03-26 00:37] falls asleep
[1518-08-24 00:12] falls asleep
[1518-08-07 00:49] wakes up
[1518-11-18 00:02] Guard #1049 begins shift
[1518-04-28 23:53] Guard #773 begins shift
[1518-05-17 00:00] Guard #773 begins shift
[1518-05-21 00:56] falls asleep
[1518-10-23 23:46] Guard #773 begins shift
[1518-08-24 00:03] Guard #1213 begins shift
[1518-04-10 00:14] falls asleep
[1518-10-27 00:28] falls asleep
[1518-06-04 00:55] falls asleep
[1518-04-02 00:29] wakes up
[1518-07-25 00:39] wakes up
[1518-09-21 23:59] Guard #97 begins shift
[1518-06-29 00:01] falls asleep
[1518-04-08 00:02] Guard #769 begins shift
[1518-11-20 00:38] falls asleep
[1518-03-25 00:42] falls asleep
[1518-08-03 00:55] wakes up
[1518-09-20 00:25] falls asleep
[1518-08-27 00:54] falls asleep
[1518-10-28 00:04] Guard #1187 begins shift
[1518-05-14 00:13] falls asleep
[1518-06-01 00:33] falls asleep
[1518-03-26 00:09] falls asleep
[1518-03-09 00:04] Guard #769 begins shift
[1518-10-16 00:40] wakes up
[1518-10-22 00:20] falls asleep
[1518-03-05 00:08] falls asleep
[1518-03-07 00:53] wakes up
[1518-05-14 00:27] falls asleep
[1518-08-13 23:59] Guard #97 begins shift
[1518-10-23 00:39] wakes up
[1518-10-22 00:02] Guard #2917 begins shift
[1518-09-26 00:53] falls asleep
[1518-10-29 00:55] wakes up
[1518-05-21 00:51] falls asleep
[1518-07-10 00:15] wakes up
[1518-08-04 23:57] Guard #1049 begins shift
[1518-04-21 00:52] wakes up
[1518-02-26 00:39] falls asleep
[1518-08-02 23:59] Guard #683 begins shift
[1518-04-10 00:53] wakes up
[1518-10-21 00:52] wakes up
[1518-08-25 00:55] falls asleep
[1518-06-29 00:57] wakes up
[1518-11-13 00:52] wakes up
[1518-05-27 00:37] falls asleep
[1518-10-24 00:02] wakes up
[1518-08-25 00:50] wakes up
[1518-05-13 00:57] falls asleep
[1518-03-25 00:00] falls asleep
[1518-10-07 23:57] Guard #2917 begins shift
[1518-05-25 00:40] wakes up
[1518-10-08 23:56] Guard #2749 begins shift
[1518-11-14 00:59] wakes up
[1518-05-17 00:53] falls asleep
[1518-05-21 00:00] Guard #1277 begins shift
[1518-04-29 00:56] wakes up
[1518-06-17 00:06] falls asleep
[1518-03-28 00:21] wakes up
[1518-03-28 00:52] wakes up
[1518-06-17 00:52] wakes up
[1518-08-21 00:40] falls asleep
[1518-08-29 00:41] falls asleep
[1518-10-29 23:52] Guard #191 begins shift
[1518-06-03 00:00] Guard #1381 begins shift
[1518-09-28 23:54] Guard #2917 begins shift
[1518-06-20 00:55] wakes up
[1518-05-17 00:58] wakes up
[1518-05-16 00:39] falls asleep
[1518-07-01 00:35] falls asleep
[1518-04-18 00:00] Guard #1049 begins shift
[1518-09-02 00:10] falls asleep
[1518-08-12 00:51] falls asleep
[1518-04-06 00:02] falls asleep
[1518-10-17 00:00] Guard #2879 begins shift
[1518-09-01 00:25] falls asleep
[1518-07-29 00:19] wakes up
[1518-02-26 00:54] wakes up
[1518-07-19 00:56] wakes up
[1518-06-02 00:56] wakes up
[1518-03-06 00:56] wakes up
[1518-04-25 00:35] falls asleep
[1518-05-09 00:00] Guard #1277 begins shift
[1518-10-04 00:57] wakes up
[1518-07-22 00:16] falls asleep
[1518-07-03 00:35] wakes up
[1518-07-22 00:46] falls asleep
[1518-08-02 00:07] falls asleep
[1518-07-17 00:44] falls asleep
[1518-06-07 00:36] wakes up
[1518-08-04 00:25] falls asleep
[1518-05-20 00:26] wakes up
[1518-07-30 23:58] Guard #1493 begins shift
[1518-03-25 00:36] wakes up
[1518-05-02 00:54] wakes up
[1518-03-09 00:19] falls asleep
[1518-03-09 00:51] wakes up
[1518-08-26 23:56] Guard #2141 begins shift
[1518-11-08 00:07] falls asleep
[1518-08-17 23:57] Guard #2917 begins shift
[1518-04-17 00:32] falls asleep
[1518-11-16 00:00] Guard #2879 begins shift
[1518-06-11 00:52] wakes up
[1518-11-10 00:55] wakes up
[1518-03-07 00:37] falls asleep
[1518-07-04 00:00] Guard #769 begins shift
[1518-03-28 00:00] Guard #2713 begins shift
[1518-04-18 00:10] falls asleep
[1518-09-24 00:48] wakes up
[1518-06-29 00:15] falls asleep
[1518-05-10 00:33] falls asleep
[1518-10-10 00:56] wakes up
[1518-03-23 00:48] falls asleep
[1518-06-05 00:55] wakes up
[1518-05-18 00:47] wakes up
[1518-10-12 23:58] Guard #2711 begins shift
[1518-07-30 00:40] falls asleep
[1518-06-24 00:02] Guard #2141 begins shift
[1518-03-03 00:48] wakes up
[1518-10-01 00:41] wakes up
[1518-06-10 00:03] Guard #1049 begins shift
[1518-02-23 00:48] wakes up
[1518-11-10 00:22] falls asleep
[1518-09-04 00:22] wakes up
[1518-05-03 00:22] falls asleep
[1518-08-05 00:32] falls asleep
[1518-04-20 00:31] wakes up
[1518-04-16 00:12] falls asleep
[1518-04-30 23:57] Guard #2711 begins shift
[1518-05-30 00:38] falls asleep
[1518-10-24 00:01] falls asleep
[1518-10-03 00:19] wakes up
[1518-05-03 23:57] Guard #2879 begins shift
[1518-11-06 00:46] falls asleep
[1518-02-27 00:37] falls asleep
[1518-11-09 23:56] Guard #2141 begins shift
[1518-08-31 00:00] Guard #1907 begins shift
[1518-03-22 00:06] falls asleep
[1518-03-30 00:59] wakes up
[1518-09-10 23:58] Guard #1493 begins shift
[1518-11-23 00:16] falls asleep
[1518-10-29 00:51] falls asleep
[1518-08-08 00:52] wakes up
[1518-04-25 00:07] falls asleep
[1518-06-29 23:46] Guard #769 begins shift
[1518-03-13 00:55] falls asleep
[1518-05-27 23:57] Guard #2113 begins shift
[1518-02-24 00:02] Guard #2879 begins shift
[1518-11-01 00:02] Guard #683 begins shift
[1518-11-06 00:55] wakes up
[1518-10-20 23:56] Guard #191 begins shift
[1518-06-14 23:58] Guard #1907 begins shift
[1518-05-23 00:04] Guard #1277 begins shift
[1518-08-01 00:11] wakes up
[1518-04-17 00:16] falls asleep
[1518-10-26 00:03] falls asleep
[1518-08-19 00:37] falls asleep
[1518-06-26 00:02] falls asleep
[1518-11-05 23:56] Guard #2879 begins shift
[1518-07-18 00:00] Guard #1049 begins shift
[1518-11-04 00:02] Guard #683 begins shift
[1518-05-01 23:59] Guard #2141 begins shift
[1518-09-23 23:47] Guard #1907 begins shift
[1518-10-06 00:45] falls asleep
[1518-02-28 23:56] Guard #2879 begins shift
[1518-06-15 23:57] Guard #769 begins shift
[1518-11-12 00:41] wakes up
[1518-05-11 23:47] Guard #1049 begins shift
[1518-11-02 00:00] Guard #683 begins shift
[1518-09-19 23:48] Guard #991 begins shift
[1518-05-24 00:45] falls asleep
[1518-11-13 00:47] falls asleep
[1518-09-20 00:05] wakes up
[1518-04-09 00:23] falls asleep
[1518-05-17 00:23] falls asleep
[1518-10-02 00:54] wakes up
[1518-09-19 00:01] Guard #769 begins shift
[1518-04-27 00:35] wakes up
[1518-11-12 00:00] Guard #2917 begins shift
[1518-04-04 00:57] wakes up
[1518-09-04 00:03] falls asleep
[1518-06-19 00:30] falls asleep
[1518-07-21 00:56] wakes up
[1518-07-16 00:17] falls asleep
[1518-10-14 00:55] wakes up
[1518-11-07 00:56] wakes up
[1518-09-17 00:00] Guard #2749 begins shift
[1518-09-16 00:27] wakes up
[1518-03-16 00:02] Guard #1907 begins shift
[1518-08-15 00:06] falls asleep
[1518-03-24 00:38] wakes up
[1518-09-18 00:00] Guard #2777 begins shift
[1518-11-20 23:56] Guard #2113 begins shift
[1518-11-23 00:02] Guard #769 begins shift
[1518-06-21 00:01] Guard #2711 begins shift
[1518-10-20 00:44] wakes up
[1518-08-15 00:32] wakes up
[1518-10-19 00:03] Guard #2711 begins shift
[1518-08-28 00:10] falls asleep
[1518-05-05 00:01] Guard #191 begins shift
[1518-11-06 00:22] wakes up
[1518-07-13 00:20] falls asleep
[1518-07-03 00:54] wakes up
[1518-03-13 23:59] Guard #1907 begins shift
[1518-04-07 00:35] wakes up
[1518-10-23 00:59] wakes up
[1518-09-15 00:16] falls asleep
[1518-09-23 00:16] falls asleep
[1518-10-23 00:00] Guard #1907 begins shift
[1518-09-28 00:13] falls asleep
[1518-08-09 00:06] falls asleep
[1518-09-19 00:54] wakes up
[1518-02-24 00:32] wakes up
[1518-09-05 00:14] falls asleep
[1518-10-26 23:59] Guard #2749 begins shift
[1518-04-05 23:53] Guard #1907 begins shift
[1518-08-09 00:46] falls asleep
[1518-04-24 00:02] Guard #1049 begins shift
[1518-03-14 00:55] wakes up
[1518-03-06 23:59] Guard #1213 begins shift
[1518-08-23 00:50] wakes up
[1518-10-12 00:41] wakes up
[1518-04-20 23:56] Guard #2777 begins shift
[1518-09-25 00:33] wakes up
[1518-10-01 00:44] falls asleep
[1518-10-02 00:53] falls asleep
[1518-04-28 00:02] Guard #769 begins shift
[1518-07-10 00:01] falls asleep
[1518-10-12 00:24] falls asleep
[1518-10-25 00:13] falls asleep
[1518-08-19 00:26] falls asleep
[1518-09-30 00:07] falls asleep
[1518-03-01 00:35] wakes up
[1518-08-16 23:57] Guard #2141 begins shift
[1518-08-18 00:14] wakes up
[1518-09-25 23:57] Guard #191 begins shift
[1518-11-14 00:42] falls asleep
[1518-07-28 23:57] Guard #1213 begins shift
[1518-10-24 00:57] wakes up
[1518-10-07 00:41] falls asleep
[1518-06-25 00:46] wakes up
[1518-06-22 00:41] wakes up
[1518-10-25 00:28] wakes up
[1518-04-05 00:30] wakes up
[1518-06-02 00:36] falls asleep
[1518-05-15 00:04] Guard #1187 begins shift
[1518-03-11 00:52] wakes up
[1518-04-27 00:03] Guard #1493 begins shift
[1518-02-24 00:22] falls asleep
[1518-06-29 00:07] wakes up
[1518-03-29 00:30] falls asleep
[1518-06-21 23:50] Guard #1049 begins shift
[1518-07-14 00:55] falls asleep
[1518-03-10 00:03] falls asleep
[1518-08-01 00:58] wakes up
[1518-06-06 00:59] wakes up
[1518-07-31 00:49] falls asleep
[1518-02-26 00:31] wakes up
[1518-08-16 00:54] wakes up
[1518-09-30 00:43] wakes up
[1518-04-09 00:59] wakes up
[1518-03-10 00:11] wakes up
[1518-11-05 00:45] falls asleep
[1518-07-02 00:03] Guard #769 begins shift
[1518-03-17 00:26] wakes up
[1518-08-13 00:02] Guard #2777 begins shift
[1518-10-25 00:03] Guard #769 begins shift
[1518-10-27 00:49] falls asleep
[1518-09-02 00:59] wakes up
[1518-07-25 00:18] falls asleep
[1518-11-12 00:21] falls asleep
[1518-06-07 00:43] wakes up
[1518-03-06 00:24] falls asleep
[1518-05-10 00:58] wakes up
[1518-08-06 23:59] Guard #1049 begins shift
[1518-09-16 00:59] wakes up
[1518-02-28 00:55] wakes up
[1518-06-23 00:34] wakes up
[1518-09-28 00:57] wakes up
[1518-05-16 00:34] wakes up
[1518-06-19 00:07] falls asleep
[1518-10-10 00:30] wakes up
[1518-10-20 00:33] falls asleep
[1518-05-12 00:00] falls asleep
[1518-04-25 00:37] wakes up
[1518-08-23 00:45] falls asleep
[1518-08-17 00:33] falls asleep
[1518-08-31 00:39] falls asleep
[1518-04-30 00:14] falls asleep
[1518-11-18 00:51] wakes up
[1518-03-16 00:07] falls asleep
[1518-09-20 00:01] falls asleep
[1518-11-09 00:39] falls asleep
[1518-05-26 00:00] Guard #769 begins shift
[1518-07-11 23:58] Guard #1213 begins shift
[1518-03-06 00:50] falls asleep
[1518-09-25 00:25] falls asleep
[1518-03-09 23:48] Guard #2141 begins shift
[1518-05-04 00:45] falls asleep
[1518-07-19 00:00] Guard #2713 begins shift
[1518-03-18 00:47] wakes up
[1518-07-08 00:42] wakes up
[1518-03-26 00:58] wakes up
[1518-08-04 00:56] wakes up
[1518-09-11 00:31] falls asleep
[1518-03-12 00:57] falls asleep
[1518-08-14 00:30] falls asleep
[1518-03-19 00:03] Guard #2879 begins shift
[1518-05-22 00:57] wakes up
[1518-05-23 00:59] wakes up
[1518-10-31 00:12] wakes up
[1518-07-03 00:47] falls asleep
[1518-09-22 23:47] Guard #991 begins shift
[1518-08-07 00:56] wakes up
[1518-11-10 23:58] Guard #1381 begins shift
[1518-08-17 00:54] wakes up
[1518-11-17 00:23] wakes up
[1518-03-01 00:16] falls asleep
[1518-06-06 23:50] Guard #1721 begins shift
[1518-06-19 00:19] wakes up
[1518-05-11 00:41] wakes up
[1518-09-12 00:58] wakes up
[1518-06-12 00:29] falls asleep
[1518-04-02 00:01] Guard #2879 begins shift
[1518-10-27 00:54] wakes up
[1518-05-14 00:14] wakes up
[1518-11-09 00:04] Guard #2917 begins shift
[1518-05-23 00:47] falls asleep
[1518-11-02 00:35] wakes up
[1518-06-30 00:51] wakes up
[1518-03-18 00:00] Guard #2749 begins shift
[1518-09-08 00:54] wakes up
[1518-05-01 00:43] falls asleep
[1518-04-11 00:54] wakes up
[1518-07-06 23:58] Guard #1213 begins shift
[1518-08-24 00:33] falls asleep
[1518-04-30 00:20] wakes up
[1518-04-28 00:14] falls asleep
[1518-02-25 23:50] Guard #2879 begins shift
[1518-09-24 00:46] falls asleep
[1518-08-12 00:55] wakes up
[1518-05-20 00:03] falls asleep
[1518-09-28 00:44] wakes up
[1518-10-11 00:35] wakes up
[1518-02-24 00:55] wakes up
[1518-05-02 23:58] Guard #2917 begins shift
[1518-09-06 00:02] Guard #191 begins shift
[1518-04-19 00:00] Guard #191 begins shift
[1518-08-21 00:11] falls asleep
[1518-07-31 23:49] Guard #683 begins shift
[1518-05-19 00:49] falls asleep
[1518-06-24 00:54] wakes up
[1518-06-29 00:53] falls asleep
[1518-11-15 00:03] Guard #2777 begins shift
[1518-08-06 00:39] falls asleep
[1518-10-16 00:21] falls asleep
[1518-04-18 00:58] wakes up
[1518-05-29 00:51] wakes up
[1518-05-06 00:36] wakes up
[1518-07-02 00:16] falls asleep
[1518-10-02 00:01] Guard #2879 begins shift
[1518-11-03 00:11] falls asleep
[1518-06-06 00:27] falls asleep
[1518-09-14 00:02] Guard #773 begins shift
[1518-04-22 00:48] wakes up
[1518-05-25 00:26] falls asleep
[1518-05-13 00:53] wakes up
[1518-07-03 00:25] falls asleep
[1518-06-27 00:57] wakes up
[1518-07-13 00:51] wakes up
[1518-11-09 00:52] wakes up
[1518-05-25 00:43] falls asleep
[1518-07-07 00:41] wakes up
[1518-05-19 23:51] Guard #2917 begins shift
[1518-10-01 00:39] falls asleep
[1518-04-15 00:37] falls asleep
[1518-08-03 00:07] falls asleep
[1518-07-14 00:57] wakes up
[1518-10-15 00:23] falls asleep
[1518-02-19 00:24] falls asleep
[1518-03-14 23:58] Guard #2879 begins shift
[1518-10-26 00:45] wakes up
[1518-07-29 00:25] wakes up
[1518-04-25 00:26] wakes up
[1518-10-20 00:25] wakes up
[1518-10-02 00:08] falls asleep
[1518-07-24 23:58] Guard #1493 begins shift
[1518-03-27 00:57] wakes up
[1518-02-23 00:52] falls asleep
[1518-08-11 00:59] wakes up
[1518-11-15 00:24] wakes up
[1518-06-18 23:57] Guard #191 begins shift
[1518-10-08 00:27] falls asleep
[1518-06-13 00:57] wakes up
[1518-04-02 00:46] falls asleep
[1518-10-24 00:52] falls asleep
[1518-05-22 00:38] wakes up
[1518-08-30 00:31] falls asleep
[1518-03-31 00:49] falls asleep
[1518-04-07 00:25] falls asleep
[1518-06-20 00:00] Guard #1721 begins shift
[1518-04-07 00:54] wakes up
[1518-10-29 00:44] wakes up
[1518-05-02 00:32] falls asleep
[1518-02-24 00:41] falls asleep
[1518-06-05 00:15] falls asleep
[1518-04-16 00:47] falls asleep
[1518-06-21 00:52] wakes up
[1518-08-29 00:59] wakes up
[1518-11-02 00:47] wakes up
[1518-11-07 00:01] Guard #1721 begins shift
[1518-10-03 00:47] falls asleep
[1518-06-04 23:58] Guard #1277 begins shift
[1518-05-09 00:13] falls asleep
[1518-07-20 00:55] falls asleep
[1518-10-30 23:53] Guard #2711 begins shift
[1518-03-22 00:00] Guard #1049 begins shift
[1518-04-08 00:14] falls asleep
[1518-03-05 00:21] wakes up
[1518-07-24 00:59] wakes up
[1518-09-12 00:18] wakes up
[1518-04-03 00:02] Guard #191 begins shift
[1518-08-14 00:58] wakes up
[1518-05-07 00:17] falls asleep
[1518-08-27 00:46] wakes up
[1518-03-17 00:04] falls asleep
[1518-10-03 00:55] wakes up
[1518-03-17 00:44] wakes up
[1518-05-10 00:34] wakes up
[1518-03-17 00:42] falls asleep
[1518-02-27 00:53] wakes up
[1518-03-14 00:53] falls asleep
[1518-02-19 00:58] wakes up
[1518-03-21 00:51] wakes up
[1518-11-04 00:59] wakes up
[1518-05-05 00:27] falls asleep
[1518-07-27 00:56] wakes up
[1518-07-26 00:17] falls asleep
[1518-05-05 00:57] wakes up
[1518-06-26 00:29] wakes up
[1518-04-08 00:50] wakes up
[1518-07-14 00:21] falls asleep
[1518-06-16 23:56] Guard #2711 begins shift
[1518-09-09 00:38] falls asleep
[1518-04-11 00:29] falls asleep
[1518-09-29 00:50] wakes up
[1518-08-30 00:41] wakes up
[1518-05-07 00:22] wakes up
[1518-07-22 00:02] Guard #1277 begins shift
[1518-03-24 00:01] Guard #1277 begins shift
[1518-07-12 00:48] wakes up
[1518-06-10 00:51] wakes up
[1518-11-05 00:52] wakes up
[1518-03-22 00:57] wakes up
[1518-08-19 00:32] wakes up
[1518-06-20 00:54] falls asleep
[1518-04-13 00:00] Guard #2113 begins shift
[1518-06-04 00:37] falls asleep
[1518-05-13 00:58] wakes up
[1518-11-05 00:51] falls asleep
[1518-06-16 00:57] wakes up
[1518-04-01 00:34] falls asleep
[1518-07-20 23:57] Guard #2713 begins shift
[1518-09-11 00:48] wakes up
[1518-09-25 00:51] falls asleep
[1518-07-29 00:24] falls asleep
[1518-06-29 00:25] wakes up
[1518-03-16 00:47] wakes up
[1518-08-06 00:59] wakes up
[1518-03-23 00:00] Guard #2879 begins shift
[1518-04-04 00:32] wakes up
[1518-10-17 00:12] falls asleep
[1518-05-06 00:11] falls asleep
[1518-11-22 00:42] falls asleep
[1518-10-18 00:28] falls asleep
[1518-02-22 00:48] falls asleep
[1518-11-15 00:11] falls asleep
[1518-04-05 00:03] Guard #1721 begins shift
[1518-05-27 00:04] Guard #2777 begins shift
[1518-06-08 00:04] Guard #1049 begins shift
[1518-08-18 00:45] wakes up
[1518-03-15 00:53] wakes up
[1518-04-26 00:03] Guard #2879 begins shift
[1518-03-29 00:54] wakes up
[1518-08-20 00:36] falls asleep
[1518-07-09 00:21] falls asleep
[1518-04-30 00:00] Guard #2711 begins shift
[1518-03-18 00:42] falls asleep
[1518-11-01 00:22] falls asleep
[1518-05-25 00:52] wakes up
[1518-03-13 00:01] Guard #769 begins shift
[1518-11-12 23:57] Guard #2879 begins shift
[1518-04-14 00:44] falls asleep
[1518-09-13 00:21] falls asleep
[1518-05-05 23:59] Guard #2917 begins shift
[1518-06-16 00:12] falls asleep
[1518-04-14 00:01] Guard #769 begins shift
[1518-07-15 23:57] Guard #2711 begins shift
[1518-08-05 00:53] wakes up
[1518-11-02 00:45] falls asleep
[1518-03-24 00:32] falls asleep
[1518-07-16 00:27] wakes up
[1518-07-09 00:53] wakes up
[1518-10-10 23:59] Guard #1277 begins shift
[1518-08-08 00:03] Guard #2879 begins shift
[1518-10-10 00:03] Guard #2917 begins shift
[1518-04-10 00:02] Guard #2711 begins shift
[1518-08-04 00:38] wakes up
[1518-05-30 00:00] Guard #2141 begins shift
[1518-06-07 00:48] falls asleep
[1518-10-05 00:21] falls asleep
[1518-02-20 00:20] wakes up
[1518-10-28 00:50] wakes up
[1518-08-19 00:03] Guard #2711 begins shift
[1518-09-18 00:26] falls asleep
[1518-05-24 00:53] wakes up
[1518-11-20 00:01] Guard #1213 begins shift
[1518-05-06 23:59] Guard #1049 begins shift
[1518-03-12 00:59] wakes up
[1518-03-01 23:58] Guard #683 begins shift
[1518-08-18 00:12] falls asleep
[1518-08-30 00:23] falls asleep
[1518-04-20 00:02] falls asleep
[1518-06-16 00:56] falls asleep
[1518-04-19 00:59] wakes up
[1518-03-30 00:52] falls asleep
[1518-10-04 00:41] falls asleep
[1518-08-13 00:57] wakes up
[1518-04-28 00:36] falls asleep
[1518-08-08 00:45] falls asleep
[1518-04-04 00:00] Guard #2711 begins shift
[1518-05-08 00:12] falls asleep
[1518-05-15 00:51] wakes up
[1518-04-29 00:40] falls asleep
[1518-09-12 00:01] falls asleep
[1518-03-05 23:57] Guard #2777 begins shift
[1518-10-09 00:47] wakes up
[1518-02-23 00:01] Guard #2749 begins shift
[1518-07-12 00:55] wakes up
[1518-11-20 00:44] wakes up
[1518-11-16 00:53] wakes up
[1518-06-26 00:37] falls asleep
[1518-07-31 00:58] wakes up
[1518-11-13 00:56] wakes up
[1518-11-14 00:33] wakes up
[1518-05-29 00:15] falls asleep
[1518-02-20 00:16] falls asleep
[1518-10-15 00:04] Guard #2917 begins shift
[1518-07-12 23:57] Guard #1277 begins shift
[1518-07-01 00:45] wakes up
[1518-06-14 00:31] falls asleep
[1518-04-23 00:43] wakes up
[1518-09-25 00:59] wakes up
[1518-07-26 00:00] Guard #1187 begins shift
[1518-06-26 00:58] wakes up
[1518-02-22 00:59] wakes up
[1518-02-21 23:58] Guard #2749 begins shift
[1518-08-30 00:28] wakes up
[1518-09-12 23:59] Guard #991 begins shift
[1518-11-01 00:26] wakes up
[1518-08-14 23:59] Guard #769 begins shift
[1518-08-24 23:50] Guard #1187 begins shift
[1518-07-15 00:38] falls asleep
[1518-11-04 00:27] falls asleep
[1518-09-21 00:19] falls asleep
[1518-11-07 23:56] Guard #1907 begins shift
[1518-06-09 00:55] wakes up
[1518-07-03 00:04] Guard #1277 begins shift
[1518-08-28 00:03] Guard #97 begins shift
[1518-07-12 00:23] falls asleep
[1518-02-24 23:57] Guard #2141 begins shift
[1518-06-18 00:01] Guard #2879 begins shift
[1518-03-01 00:48] falls asleep
[1518-09-21 00:04] Guard #2917 begins shift
[1518-06-13 00:39] falls asleep
[1518-09-22 00:14] falls asleep
[1518-06-21 00:27] falls asleep
[1518-07-19 23:59] Guard #2777 begins shift
[1518-04-09 00:02] Guard #2711 begins shift
[1518-09-16 00:16] falls asleep
[1518-10-03 00:53] falls asleep
[1518-10-27 00:23] wakes up
[1518-03-28 00:06] falls asleep
[1518-07-04 00:42] wakes up
[1518-09-08 00:00] Guard #2879 begins shift
[1518-11-15 00:56] wakes up
[1518-06-28 00:09] wakes up
[1518-09-06 00:50] wakes up
[1518-04-17 00:03] Guard #1721 begins shift
[1518-05-10 23:46] Guard #1213 begins shift
[1518-08-21 00:55] falls asleep
[1518-06-09 00:03] Guard #97 begins shift
[1518-11-14 00:11] falls asleep
[1518-06-07 00:05] falls asleep
[1518-06-11 00:01] Guard #1049 begins shift
[1518-04-22 00:18] falls asleep
[1518-03-27 00:25] falls asleep
[1518-09-08 23:47] Guard #2777 begins shift
[1518-05-31 00:13] falls asleep
[1518-07-27 00:52] falls asleep
[1518-03-20 23:56] Guard #1493 begins shift
[1518-10-22 00:54] wakes up
[1518-08-07 00:52] falls asleep
[1518-03-30 00:53] wakes up
[1518-10-25 23:54] Guard #2711 begins shift
[1518-08-19 23:50] Guard #683 begins shift
[1518-04-19 00:52] falls asleep
[1518-07-20 00:56] wakes up
[1518-04-27 00:40] falls asleep
[1518-11-14 00:02] Guard #1493 begins shift
[1518-07-19 00:51] falls asleep
[1518-04-27 00:13] falls asleep
[1518-08-18 00:35] falls asleep
[1518-05-26 00:37] wakes up
[1518-08-15 23:50] Guard #1277 begins shift
[1518-09-09 00:23] wakes up
[1518-07-06 00:38] wakes up
[1518-04-17 00:43] wakes up
[1518-03-03 00:00] Guard #1721 begins shift
[1518-09-17 00:48] wakes up
[1518-03-03 00:19] falls asleep
[1518-10-31 00:50] falls asleep
[1518-04-24 00:41] wakes up
[1518-02-26 00:04] falls asleep
[1518-05-15 23:56] Guard #2879 begins shift
[1518-08-25 00:05] falls asleep
[1518-08-23 00:01] Guard #1277 begins shift
[1518-04-12 00:38] wakes up
[1518-10-10 00:28] falls asleep
[1518-10-01 00:48] wakes up
[1518-11-19 00:42] falls asleep
[1518-09-10 00:00] Guard #769 begins shift
[1518-05-22 00:55] falls asleep
[1518-04-24 00:31] falls asleep
[1518-10-16 00:49] falls asleep
[1518-05-11 00:49] falls asleep
[1518-04-06 00:32] falls asleep
[1518-09-29 00:22] wakes up
[1518-06-24 00:42] falls asleep
[1518-06-30 23:53] Guard #2713 begins shift
[1518-08-22 00:01] Guard #1277 begins shift
[1518-11-06 00:16] falls asleep
[1518-11-02 23:56] Guard #2917 begins shift
[1518-08-29 00:03] Guard #1049 begins shift
[1518-03-11 00:10] falls asleep
[1518-10-16 00:00] Guard #191 begins shift
[1518-02-25 00:53] wakes up
[1518-02-25 00:42] falls asleep
[1518-05-12 00:56] wakes up
[1518-04-15 00:25] falls asleep
[1518-08-19 00:51] wakes up
[1518-11-18 23:57] Guard #2879 begins shift
[1518-07-22 23:56] Guard #2713 begins shift
[1518-04-11 23:56] Guard #191 begins shift
[1518-05-23 00:37] wakes up
[1518-09-30 00:08] wakes up
[1518-04-14 00:53] wakes up
[1518-04-28 00:53] wakes up
[1518-08-20 00:26] wakes up
[1518-08-02 00:55] wakes up
[1518-05-12 23:58] Guard #1493 begins shift
[1518-05-13 00:44] falls asleep
[1518-09-07 00:35] falls asleep
[1518-05-31 00:23] falls asleep
[1518-07-02 00:44] wakes up
[1518-03-31 00:55] wakes up
[1518-10-30 00:01] falls asleep
[1518-03-19 00:10] falls asleep
[1518-07-04 00:21] falls asleep
[1518-05-31 00:04] Guard #1213 begins shift
[1518-10-27 00:46] wakes up
[1518-08-20 23:56] Guard #2141 begins shift
[1518-11-15 00:55] falls asleep
[1518-06-08 00:58] wakes up
[1518-09-14 00:53] wakes up
[1518-06-07 00:41] falls asleep
[1518-07-26 00:40] wakes up
[1518-10-01 00:00] Guard #2749 begins shift
[1518-08-07 00:22] falls asleep
[1518-05-03 00:53] falls asleep
[1518-05-16 00:54] wakes up
[1518-08-05 23:59] Guard #2917 begins shift
[1518-03-26 00:00] Guard #2749 begins shift
[1518-04-14 00:41] wakes up
[1518-05-14 00:50] wakes up
[1518-08-26 00:13] falls asleep
[1518-04-19 00:36] falls asleep
[1518-09-24 23:57] Guard #191 begins shift
[1518-02-27 00:11] falls asleep
[1518-10-20 00:21] falls asleep
[1518-04-04 00:14] falls asleep
[1518-03-01 00:51] wakes up
[1518-07-18 00:54] wakes up
[1518-09-30 00:12] falls asleep
[1518-05-21 00:58] wakes up
[1518-06-01 00:11] wakes up
[1518-04-01 00:56] wakes up
[1518-08-20 00:04] falls asleep
[1518-10-08 00:58] wakes up
[1518-11-13 00:55] falls asleep
[1518-03-08 00:55] wakes up
[1518-08-21 00:45] wakes up
[1518-10-18 00:03] Guard #2879 begins shift
[1518-04-25 00:01] Guard #1493 begins shift
[1518-11-02 00:16] falls asleep
[1518-05-31 23:54] Guard #2879 begins shift
[1518-06-01 00:05] falls asleep
[1518-03-08 00:29] falls asleep
[1518-05-23 00:31] falls asleep
[1518-09-09 00:57] wakes up
[1518-05-01 00:51] wakes up
[1518-11-08 00:54] wakes up
[1518-07-15 00:01] Guard #1213 begins shift
[1518-06-09 00:54] falls asleep
[1518-03-13 00:50] wakes up
[1518-08-04 00:48] falls asleep
[1518-06-04 00:44] wakes up
[1518-05-27 00:19] wakes up
[1518-08-11 23:59] Guard #991 begins shift
[1518-04-06 00:47] falls asleep
[1518-06-13 00:02] Guard #1187 begins shift
[1518-06-16 00:48] wakes up
[1518-08-10 00:55] wakes up
[1518-04-18 00:35] falls asleep
[1518-08-28 00:18] falls asleep
[1518-06-12 00:53] wakes up
[1518-04-06 00:42] wakes up
[1518-07-10 23:58] Guard #1721 begins shift
[1518-09-03 00:01] Guard #2917 begins shift
[1518-04-29 00:03] falls asleep
[1518-07-09 00:00] Guard #191 begins shift
[1518-04-26 00:54] wakes up
[1518-08-13 00:23] falls asleep
[1518-06-01 23:56] Guard #1277 begins shift
[1518-10-12 00:01] Guard #1187 begins shift
[1518-07-17 00:38] falls asleep
[1518-04-01 00:01] Guard #1049 begins shift
[1518-03-31 00:34] wakes up
[1518-04-22 00:02] Guard #2917 begins shift
[1518-09-23 00:47] wakes up
[1518-06-22 23:59] Guard #2777 begins shift
[1518-02-20 00:01] Guard #2777 begins shift
[1518-06-06 00:02] Guard #769 begins shift
[1518-09-28 00:03] Guard #2777 begins shift
[1518-03-29 00:02] Guard #1277 begins shift
[1518-10-23 00:09] falls asleep
[1518-07-14 00:45] wakes up
[1518-04-27 00:53] wakes up
[1518-09-17 00:11] falls asleep
[1518-11-18 00:24] falls asleep
[1518-08-10 23:58] Guard #1213 begins shift
[1518-02-26 23:57] Guard #769 begins shift
[1518-10-06 00:00] Guard #2749 begins shift
[1518-02-26 00:41] wakes up
[1518-07-01 00:25] wakes up
[1518-08-22 00:45] wakes up
[1518-04-29 00:04] wakes up
[1518-03-04 00:46] falls asleep
[1518-05-09 00:37] wakes up
[1518-05-22 00:02] Guard #1907 begins shift
[1518-04-12 00:22] falls asleep
[1518-07-11 00:28] falls asleep
[1518-09-11 23:48] Guard #1721 begins shift
[1518-07-17 00:39] wakes up
[1518-08-27 00:24] falls asleep
[1518-10-15 00:39] wakes up
[1518-09-22 00:41] wakes up
[1518-08-28 00:34] wakes up
[1518-04-05 00:14] falls asleep
[1518-02-23 00:31] falls asleep
[1518-03-23 00:53] wakes up
[1518-09-09 00:05] falls asleep
[1518-09-13 00:36] wakes up
[1518-10-13 00:33] falls asleep
[1518-10-29 00:27] falls asleep
[1518-07-22 00:52] wakes up
[1518-03-16 23:49] Guard #97 begins shift
[1518-10-05 00:04] Guard #2917 begins shift
[1518-10-16 00:59] wakes up
[1518-11-08 00:22] wakes up
[1518-07-31 00:10] falls asleep
[1518-10-07 00:59] wakes up
[1518-09-23 00:06] wakes up
[1518-05-27 00:58] wakes up
[1518-04-15 00:00] Guard #1049 begins shift
[1518-02-23 00:53] wakes up
[1518-10-07 00:50] wakes up
[1518-08-24 00:41] wakes up
[1518-09-01 00:58] wakes up
[1518-09-08 00:49] falls asleep
[1518-08-25 00:06] wakes up
[1518-08-09 00:40] wakes up
[1518-09-10 00:34] falls asleep
[1518-04-15 00:30] wakes up
[1518-07-27 00:03] Guard #1907 begins shift
[1518-02-20 00:53] wakes up
[1518-06-04 00:56] wakes up
[1518-06-27 23:56] Guard #191 begins shift
[1518-03-24 23:47] Guard #991 begins shift
[1518-04-23 00:26] falls asleep
[1518-06-25 23:50] Guard #1049 begins shift
[1518-08-15 00:35] falls asleep
[1518-06-23 00:27] falls asleep
[1518-11-19 00:57] wakes up
[1518-06-14 00:56] wakes up
[1518-03-28 00:57] falls asleep
[1518-07-11 00:45] wakes up
[1518-11-16 23:50] Guard #1493 begins shift
[1518-05-27 00:17] falls asleep
[1518-02-27 00:13] wakes up
[1518-07-19 00:15] falls asleep
[1518-09-30 00:01] Guard #1049 begins shift
[1518-03-25 00:50] wakes up
[1518-03-26 23:59] Guard #2141 begins shift
[1518-05-09 23:58] Guard #1049 begins shift
[1518-10-19 23:56] Guard #1213 begins shift
[1518-10-26 00:59] wakes up
[1518-07-20 00:19] wakes up
[1518-05-09 00:48] falls asleep
[1518-03-28 00:50] falls asleep
[1518-09-26 00:56] wakes up
[1518-07-29 00:12] falls asleep
[1518-04-06 23:59] Guard #2749 begins shift
[1518-10-29 00:00] Guard #1187 begins shift
[1518-06-11 00:25] falls asleep
[1518-07-18 00:29] falls asleep
[1518-09-24 00:51] falls asleep
[1518-08-25 00:57] wakes up
[1518-05-26 00:06] falls asleep
[1518-07-17 00:01] Guard #1907 begins shift
[1518-06-26 23:59] Guard #1277 begins shift
[1518-05-19 00:46] wakes up
[1518-09-26 23:56] Guard #1187 begins shift
[1518-04-03 00:29] wakes up
[1518-04-23 00:00] Guard #1277 begins shift
[1518-07-06 00:25] falls asleep
[1518-11-15 00:52] wakes up
[1518-06-28 00:07] falls asleep
[1518-10-19 00:14] falls asleep
[1518-10-30 00:33] wakes up
[1518-09-21 00:51] wakes up
[1518-09-16 00:49] falls asleep
[1518-04-19 00:49] wakes up
[1518-09-12 00:23] falls asleep
[1518-07-19 00:33] wakes up
[1518-03-26 00:48] wakes up
[1518-04-16 00:02] Guard #2777 begins shift
[1518-10-31 00:52] wakes up
[1518-07-20 00:42] falls asleep
[1518-07-04 23:58] Guard #1213 begins shift
[1518-08-16 00:00] falls asleep
[1518-08-29 23:56] Guard #1213 begins shift
[1518-07-09 23:49] Guard #991 begins shift
[1518-11-05 00:00] Guard #2711 begins shift
[1518-11-03 00:43] wakes up
[1518-11-05 00:48] wakes up
[1518-09-16 00:00] Guard #97 begins shift
[1518-11-18 00:50] falls asleep
[1518-08-27 00:59] wakes up
[1518-11-17 00:01] falls asleep
[1518-05-16 00:08] falls asleep
[1518-06-27 00:20] falls asleep
[1518-08-09 00:59] wakes up
[1518-04-16 00:23] wakes up
[1518-06-20 00:45] wakes up
[1518-08-15 00:56] wakes up
[1518-09-01 23:59] Guard #1721 begins shift
[1518-04-14 00:35] falls asleep
[1518-02-21 00:09] falls asleep
[1518-11-09 00:41] wakes up
[1518-05-08 00:55] wakes up
[1518-05-22 00:31] falls asleep
[1518-10-23 00:52] falls asleep
[1518-09-24 00:02] falls asleep
[1518-08-21 00:57] wakes up
[1518-11-23 00:49] wakes up
[1518-06-01 00:55] wakes up
[1518-03-26 00:51] falls asleep
[1518-03-08 00:00] Guard #2749 begins shift
[1518-09-07 00:01] Guard #1493 begins shift
[1518-05-19 00:02] Guard #2749 begins shift
[1518-10-02 00:14] wakes up
[1518-03-02 00:11] falls asleep
[1518-09-12 00:56] falls asleep
[1518-07-23 00:23] falls asleep
[1518-06-07 00:51] wakes up
[1518-06-18 00:21] falls asleep
[1518-09-28 00:48] falls asleep
[1518-06-30 00:01] falls asleep
[1518-03-20 00:31] falls asleep
[1518-09-05 00:51] wakes up
[1518-05-29 00:00] Guard #2713 begins shift
[1518-10-21 00:40] falls asleep
[1518-08-10 00:33] falls asleep
[1518-07-05 00:53] falls asleep
[1518-10-03 00:09] falls asleep
[1518-03-20 00:00] Guard #2777 begins shift
[1518-04-18 00:21] wakes up
[1518-07-29 23:56] Guard #2141 begins shift
[1518-05-20 00:33] falls asleep
[1518-08-20 00:42] wakes up
[1518-05-21 00:52] wakes up
[1518-03-10 23:58] Guard #2777 begins shift
[1518-04-26 00:50] falls asleep
[1518-04-04 00:47] falls asleep
[1518-07-07 00:12] falls asleep
[1518-10-07 00:55] falls asleep
[1518-09-20 00:39] wakes up"""
