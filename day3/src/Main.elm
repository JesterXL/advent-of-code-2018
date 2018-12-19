{- 

    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden

    https://adventofcode.com/2018/day/3

-}

module Main exposing (main)

import Array exposing (Array)
import Browser
import Char exposing (fromCode, toCode)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, foldl, head, length, map, partition, reverse)
import Maybe exposing (withDefault)
import Set exposing (Set, empty, insert)
import String exposing (split, toList)
import Tuple exposing (first, pair)
import Canvas exposing (..)
import CanvasColor as Color exposing (Color)

type alias Claim =
    { id : Int
    , rectangle : Rectangle
    , calculated : Bool }

type alias Rectangle =
    { left : Int
    , top : Int
    , width : Int
    , height : Int
    , right : Int
    , bottom : Int }

type alias Model =
    { claimsText : String -- big ole string full of checksums
    , claims : Array Claim -- how many 2's and 3's are there multipled together
    , squareInches : Int
    , allOverlappingRectangles : Array (Array Rectangle)
    }

initialModel : Model
initialModel =
    { claimsText = ""
    , claims = Array.empty
    , squareInches = 0
    , allOverlappingRectangles = Array.empty
    }


type Msg
    = InputClaimsText String -- when you type or paste in the text area
    | ParseClaimsText -- RUN THE MAGIC
    | LoadFromCache -- call the checksums function to load from the code vs. copy pasta


-- [Challenge 1 Functions] ------------------------------------------------------------

getRectangle : Int -> Int -> Int -> Int -> Rectangle
getRectangle left top width height =
    Rectangle left top width height (left + width) (top + height)

getClaim : Int -> Int -> Int -> Int -> Int -> Claim
getClaim id left top width height =
    Claim id (getRectangle left top width height) False

parseClaims =
    split "\n" claimsCacheString
        |> Array.fromList
        |> Array.map (split " ")
        |> Array.map Array.fromList


parseClaimStrings stringArray =
    let
        id =
            Array.get 0 stringArray
            |> Maybe.withDefault "#0"
            |> String.replace "#" ""
            |> String.toInt
            |> Maybe.withDefault 0

        left =
            Array.get 2 stringArray
            |> Maybe.withDefault ","
            |> String.split ","
            |> Array.fromList
            |> Array.get 0
            |> Maybe.withDefault "0"
            |> String.toInt
            |> Maybe.withDefault 0
        top =
            Array.get 2 stringArray
            |> Maybe.withDefault ","
            |> String.split ","
            |> Array.fromList
            |> Array.get 1
            |> Maybe.withDefault "0"
            |> String.replace ":" ""
            |> String.toInt
            |> Maybe.withDefault 0

        width =
            Array.get 3 stringArray
            |> Maybe.withDefault "x"
            |> String.split "x"
            |> Array.fromList
            |> Array.get 0
            |> Maybe.withDefault "0"
            |> String.toInt
            |> Maybe.withDefault 0

        height =
            Array.get 3 stringArray
            |> Maybe.withDefault "x"
            |> String.split "x"
            |> Array.fromList
            |> Array.get 1
            |> Maybe.withDefault "0"
            |> String.toInt
            |> Maybe.withDefault 0

    in
        Claim id (getRectangle left top width height) False

canvasBackgroundColor =
    -- Color.rgb 63 81 181
    Color.rgb 26 35 126

canvasClaimColor =
    -- Color.rgb 233 30 99
    Color.rgb 136 14 79

canvasOverlapColor =
    Color.rgb 233 30 99

renderBackground cmds =
    cmds
        |> Canvas.fillStyle canvasBackgroundColor
        |> Canvas.fillRect 0 0 1000 1000

renderEmptyRectangle x y width height cmds =
    cmds
        |> Canvas.lineWidth 2
        |> Canvas.strokeStyle canvasClaimColor
        |> Canvas.strokeRect (toFloat x) (toFloat y) (toFloat width) (toFloat height)

renderFilledRectangle x y width height color cmds =
    cmds
        |> Canvas.fillStyle color
        |> Canvas.fillRect (toFloat x) (toFloat y) (toFloat width) (toFloat height)

renderClaim claim cmds =
    -- let
    --     logClaim = log "claim" claim
    --     logCmds = log "cmds" cmds
    -- in
    cmds
        |> renderEmptyRectangle claim.rectangle.left claim.rectangle.top claim.rectangle.width claim.rectangle.height

renderOverlap rectangle cmds =
    cmds
        |> renderFilledRectangle rectangle.left rectangle.top rectangle.width rectangle.height canvasOverlapColor

claimsOverlap : Claim -> Claim -> Bool
claimsOverlap claim1 claim2 =
    let
        rectangle1 = claim1.rectangle
        rectangle2 = claim2.rectangle
    in
        if rectangle1.left > rectangle2.right || rectangle2.left > rectangle1.right then
            False
        else if rectangle1.bottom < rectangle2.top || rectangle2.bottom < rectangle1.top then
            False
        else  
            True

getClaimOverlap : Claim -> Claim -> Rectangle
getClaimOverlap claim1 claim2 =
    if claimsOverlap claim1 claim2 then
        let
            rect1 = claim1.rectangle
            rect2 = claim2.rectangle
            
            minRight = min rect1.right rect2.right
            maxLeft = max rect1.left rect2.left
            xOverlap =  max 0 (minRight - maxLeft)

            minBottom = min rect1.bottom rect2.bottom
            maxTop = max rect1.top rect2.top
            yOverlap =  max 0 (minBottom - maxTop)
            -- rect1Log = log "rect1" rect1
            -- rect2Log = log "rect2" rect2
            -- xOverlapLog = log "xOverlap" xOverlap
            -- yOverlapLog = log "yOverlap" yOverlap
        in
            getRectangle (max rect1.left rect2.left) (max rect1.top rect2.top) xOverlap yOverlap
    else
        getRectangle 0 0 0 0

rectangleBiggerThanZero : Rectangle -> Bool
rectangleBiggerThanZero rectangle =
    if rectangle.width > 0 || rectangle.height > 0 then
        True
    else
        False

someArentCalculatedYet : Claim -> Claim -> Bool
someArentCalculatedYet claim1 claim2 =
    if claim1.calculated == False || claim2.calculated == False then
        True
    else
        False
    
getClaimsOverlapped claims claim1 =
    Array.foldl (\claim acc ->
        if (someArentCalculatedYet claim1 claim) && claim.id /= claim1.id && claimsOverlap claim1 claim == True then
            Array.push (getClaimOverlap claim1 claim) acc
        else
            acc) (Array.fromList []) claims

combineTwoRectangles : Rectangle -> Rectangle -> Rectangle
combineTwoRectangles rect1 rect2 =
    getRectangle 0 0 (rect1.width + rect2.width) (rect1.height + rect2.height)

combineAllRectangles : Array Rectangle -> Rectangle
combineAllRectangles rectangles =
    Array.foldl (\rect acc -> combineTwoRectangles rect acc) (getRectangle 0 0 0 0) rectangles

calculateTotalArea : Rectangle -> Int
calculateTotalArea rectangle =
    rectangle.width * rectangle.height

-- attempt 2

buildIntArray : Int -> Array Int
buildIntArray len =
    Array.initialize len (always 0)

type alias Grid =
    { rows : Int
    , cols : Int
    , grid : Array (Array Int)
    }

buildGrid : Int -> Int -> Grid
buildGrid rows cols =
    buildIntArray rows
    |> Array.map (\row -> buildIntArray cols)
    |> Grid rows cols

intInRange : Int -> Int -> Int -> Bool
intInRange start end num =
    if num >= start && num <= end then
        True
    else
        False

intIn1000Range : Int -> Bool
intIn1000Range num =
    intInRange 0 999 num

getRowFromRectangle : Int -> Rectangle -> Array Int
getRowFromRectangle cols rectangle =
    let
        intInClaimRange =
            intInRange rectangle.left rectangle.right
    in
        buildIntArray cols
        |> Array.indexedMap (\index value -> if (intInClaimRange index) then value + 1 else value)

getRowsFromRectangle : Int -> Rectangle -> Array (Array Int)   
getRowsFromRectangle cols rectangle =
    Array.repeat rectangle.height 0
    |> Array.map (\_ -> getRowFromRectangle cols rectangle)

getIntOrZero : Int -> Array Int -> Int
getIntOrZero index array =
    Array.get index array
    |> Maybe.withDefault 0

sumArrays : Array Int -> Array Int -> Array Int
sumArrays array1 array2 =
    Array.indexedMap (\index value -> (getIntOrZero index array1) + value) array2

getRowOrDefaultZeroArray : Array (Array Int) -> Int -> Int -> Array Int
getRowOrDefaultZeroArray demRects index length =
    Array.get index demRects
    |> Maybe.withDefault (buildIntArray length)


updateGrid : Grid -> Array (Array Int) -> Int -> Int -> Grid 
updateGrid grid demRects startingRow endingRow =
    { grid | grid = Array.indexedMap (\index row -> 
        if (intInRange startingRow endingRow index) == True then
            let
                indexToGetRow = index - startingRow
                updaterRow = getRowOrDefaultZeroArray demRects indexToGetRow grid.rows 
            in
            sumArrays row updaterRow
        else
            row) grid.grid }

flattenRectangleArray : Array Rectangle -> Array Rectangle -> Array Rectangle
flattenRectangleArray rectangles accumlator =
    Array.foldl (\rect acc -> Array.push rect acc) accumlator rectangles

flattenRectangleMDArray : Array (Array Rectangle) -> Array Rectangle
flattenRectangleMDArray rectanglesList =
    Array.foldl (\rects acc -> flattenRectangleArray rects acc) Array.empty rectanglesList

update : Msg -> Model -> Model
update msg model =
    case msg of
        -- do the magic
        ParseClaimsText ->
            let
                -- Attempt #1: 281701600 is too high, however, I figured as much. I'm not removing 2 matches, and instead
                -- keeping them around; I need to remove them.
                rowsAmount = 1000
                colsAmount = 1000

                claims = parseClaims
                    |> Array.map parseClaimStrings
                    -- |> Array.slice 0 100
                -- strLog = log "claims" claims

                grid = 
                    buildGrid rowsAmount colsAmount

                allOverlappingRectangles =
                    Array.map (\claim -> getClaimsOverlapped claims claim) claims
                    |> Array.filter (\overlappedRectangles -> Array.isEmpty overlappedRectangles == False)
                -- allOverlappingRectanglesLog = log "allOverlappingRectangles" allOverlappingRectangles


                firstRectangle = 
                    Array.get 0 allOverlappingRectangles
                    |> Maybe.withDefault (Array.fromList [getRectangle 0 0 0 0])
                    |> Array.get 0
                    |> Maybe.withDefault (getRectangle 0 0 0 0)

                demRects = getRowsFromRectangle colsAmount firstRectangle
                -- demRectsLog = log "demRects" demRects
                
                -- newGrid = updateGrid grid demRects firstRectangle.top firstRectangle.bottom
                -- newGridLog = log "newGrid" newGrid

                datFreshGrid =
                    Array.map (\rectList -> Array.map (\rect -> updateGrid grid demRects rect.top rect.bottom) rectList ) allOverlappingRectangles
                -- datFreshGridLog = log "datFreshGrid" datFreshGrid
                datFreshGridLog = log "datFreshGrid" "done"


            in
            { model | claims = claims, allOverlappingRectangles = allOverlappingRectangles }

        -- when you type or copy pasta into the text area
        InputClaimsText text ->
            { model | claimsText = text }

        -- put the checksums function text into the text area
        LoadFromCache ->
            { model | claimsText = claimsCacheString }

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
                        , onInput InputClaimsText
                        ]
                        [ text model.claimsText ]
                    ]
                , div [][
                        Canvas.element
                            1000
                            1000
                            [ style "border" "1px solid black"]
                            ( Canvas.empty
                                |> Canvas.clearRect 0 0 1000 1000
                                |> renderBackground
                                -- |> (\cmds -> Array.foldl (renderClaim cmds) Claim 1 2 3 4 6)
                                |> (\cmds -> Array.foldl renderClaim cmds model.claims)
                                |> (\cmds -> Array.foldl renderOverlap cmds (flattenRectangleMDArray model.allOverlappingRectangles))
                                )
                    ]
                ]
            , div [ class "mdl-card__actions mdl-card--border" ]
                [ a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick LoadFromCache
                    ]
                    [ text "Load Cached" ]
                , a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                    , onClick ParseClaimsText
                    ]
                    [ text "Parse Claims" ]
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

claimsCacheString : String
claimsCacheString =
    """#1 @ 850,301: 23x12
#2 @ 898,245: 15x10
#3 @ 8,408: 12x27
#4 @ 532,184: 16x13
#5 @ 550,829: 11x10
#6 @ 656,906: 13x12
#7 @ 489,357: 24x23
#8 @ 529,898: 12x19
#9 @ 660,201: 19x28
#10 @ 524,14: 21x27
#11 @ 211,206: 13x23
#12 @ 571,71: 18x12
#13 @ 934,447: 26x11
#14 @ 851,648: 13x15
#15 @ 459,123: 18x27
#16 @ 572,613: 11x22
#17 @ 913,502: 29x20
#18 @ 419,691: 14x26
#19 @ 813,147: 10x13
#20 @ 435,286: 19x20
#21 @ 660,678: 17x13
#22 @ 806,575: 16x24
#23 @ 247,512: 19x21
#24 @ 112,345: 28x25
#25 @ 865,936: 13x15
#26 @ 6,725: 23x10
#27 @ 531,544: 28x17
#28 @ 533,393: 10x13
#29 @ 17,926: 23x11
#30 @ 962,637: 29x22
#31 @ 934,76: 11x12
#32 @ 607,278: 10x11
#33 @ 482,144: 28x19
#34 @ 767,721: 12x23
#35 @ 797,609: 12x12
#36 @ 134,687: 24x26
#37 @ 106,102: 19x25
#38 @ 303,385: 11x16
#39 @ 626,25: 12x29
#40 @ 369,895: 11x27
#41 @ 656,740: 12x22
#42 @ 949,338: 12x21
#43 @ 629,360: 21x18
#44 @ 416,388: 10x17
#45 @ 70,536: 16x12
#46 @ 54,758: 27x18
#47 @ 441,756: 13x24
#48 @ 775,49: 13x25
#49 @ 145,779: 26x25
#50 @ 857,202: 10x24
#51 @ 835,731: 20x14
#52 @ 811,130: 18x21
#53 @ 585,62: 26x29
#54 @ 277,976: 18x16
#55 @ 577,523: 24x25
#56 @ 24,401: 18x15
#57 @ 524,674: 25x21
#58 @ 654,237: 26x19
#59 @ 825,31: 17x29
#60 @ 452,528: 24x11
#61 @ 123,161: 14x11
#62 @ 921,366: 27x12
#63 @ 300,312: 15x22
#64 @ 276,403: 18x23
#65 @ 27,660: 16x28
#66 @ 229,222: 16x29
#67 @ 526,724: 24x23
#68 @ 414,278: 20x22
#69 @ 75,634: 11x19
#70 @ 552,646: 25x21
#71 @ 878,406: 28x23
#72 @ 134,492: 15x28
#73 @ 152,12: 11x20
#74 @ 158,967: 11x22
#75 @ 102,795: 24x29
#76 @ 745,962: 12x16
#77 @ 324,946: 11x14
#78 @ 916,317: 28x28
#79 @ 667,226: 18x27
#80 @ 168,286: 11x26
#81 @ 403,666: 20x28
#82 @ 653,245: 23x10
#83 @ 238,479: 23x14
#84 @ 789,915: 14x3
#85 @ 404,761: 16x22
#86 @ 418,925: 12x12
#87 @ 206,977: 22x19
#88 @ 134,152: 29x23
#89 @ 754,417: 12x17
#90 @ 341,674: 17x18
#91 @ 542,145: 12x25
#92 @ 355,758: 17x21
#93 @ 381,177: 29x11
#94 @ 944,803: 26x18
#95 @ 459,35: 7x6
#96 @ 870,845: 24x26
#97 @ 85,744: 10x13
#98 @ 270,485: 10x23
#99 @ 118,589: 10x26
#100 @ 694,716: 10x21
#101 @ 662,914: 26x17
#102 @ 910,46: 14x24
#103 @ 820,788: 18x11
#104 @ 229,369: 22x26
#105 @ 28,920: 14x18
#106 @ 901,469: 15x13
#107 @ 607,929: 12x26
#108 @ 17,755: 18x29
#109 @ 512,692: 20x14
#110 @ 424,375: 27x28
#111 @ 63,641: 26x28
#112 @ 253,475: 24x27
#113 @ 89,43: 21x16
#114 @ 23,112: 24x18
#115 @ 893,207: 24x11
#116 @ 852,221: 15x28
#117 @ 174,55: 13x22
#118 @ 9,796: 13x26
#119 @ 564,120: 21x12
#120 @ 333,974: 29x16
#121 @ 353,559: 15x27
#122 @ 397,954: 10x14
#123 @ 20,891: 19x28
#124 @ 935,26: 11x28
#125 @ 861,118: 24x27
#126 @ 667,864: 10x28
#127 @ 257,217: 25x29
#128 @ 737,945: 21x29
#129 @ 579,552: 10x27
#130 @ 390,280: 20x16
#131 @ 597,293: 16x13
#132 @ 664,234: 24x18
#133 @ 775,44: 29x14
#134 @ 559,890: 24x19
#135 @ 234,751: 10x23
#136 @ 264,34: 28x29
#137 @ 279,118: 19x19
#138 @ 453,283: 20x19
#139 @ 683,45: 23x27
#140 @ 774,503: 23x10
#141 @ 732,50: 22x24
#142 @ 492,600: 20x14
#143 @ 530,911: 10x27
#144 @ 829,654: 29x23
#145 @ 311,612: 29x11
#146 @ 802,58: 23x18
#147 @ 570,688: 27x25
#148 @ 653,143: 25x12
#149 @ 425,221: 10x11
#150 @ 158,711: 20x13
#151 @ 278,611: 3x11
#152 @ 561,309: 10x20
#153 @ 883,49: 21x13
#154 @ 964,694: 19x22
#155 @ 769,928: 12x22
#156 @ 237,584: 16x27
#157 @ 845,378: 29x12
#158 @ 248,958: 22x10
#159 @ 840,548: 20x21
#160 @ 13,646: 16x10
#161 @ 747,957: 21x29
#162 @ 227,785: 24x20
#163 @ 951,87: 29x12
#164 @ 651,836: 10x24
#165 @ 779,672: 15x23
#166 @ 888,379: 26x17
#167 @ 344,64: 14x22
#168 @ 375,397: 10x6
#169 @ 859,103: 20x14
#170 @ 677,4: 23x27
#171 @ 835,205: 28x16
#172 @ 245,775: 10x20
#173 @ 400,358: 27x26
#174 @ 29,230: 11x24
#175 @ 861,202: 20x15
#176 @ 831,758: 21x12
#177 @ 351,612: 21x29
#178 @ 603,367: 23x21
#179 @ 603,337: 20x28
#180 @ 440,494: 14x26
#181 @ 366,834: 17x11
#182 @ 17,310: 15x16
#183 @ 839,319: 21x25
#184 @ 361,696: 22x10
#185 @ 822,457: 24x18
#186 @ 920,639: 19x29
#187 @ 921,673: 29x24
#188 @ 70,705: 23x13
#189 @ 612,150: 26x19
#190 @ 206,219: 25x21
#191 @ 489,139: 17x12
#192 @ 187,777: 24x22
#193 @ 168,47: 28x21
#194 @ 637,743: 12x24
#195 @ 536,735: 17x29
#196 @ 108,584: 10x29
#197 @ 661,882: 14x12
#198 @ 747,61: 11x10
#199 @ 268,44: 17x13
#200 @ 319,299: 25x19
#201 @ 676,600: 19x10
#202 @ 726,855: 16x14
#203 @ 365,318: 11x22
#204 @ 863,95: 10x29
#205 @ 131,782: 11x25
#206 @ 737,638: 23x12
#207 @ 708,580: 18x12
#208 @ 71,966: 25x15
#209 @ 672,210: 18x26
#210 @ 206,657: 14x26
#211 @ 973,197: 10x11
#212 @ 83,880: 14x19
#213 @ 69,769: 22x16
#214 @ 136,606: 28x23
#215 @ 635,585: 16x14
#216 @ 900,231: 13x28
#217 @ 180,620: 11x28
#218 @ 836,457: 12x28
#219 @ 71,595: 12x11
#220 @ 270,605: 17x26
#221 @ 32,901: 29x13
#222 @ 153,911: 29x17
#223 @ 6,629: 25x22
#224 @ 21,629: 22x29
#225 @ 280,395: 29x17
#226 @ 483,302: 27x15
#227 @ 854,388: 15x21
#228 @ 873,377: 24x11
#229 @ 405,831: 24x16
#230 @ 220,489: 15x23
#231 @ 115,267: 22x19
#232 @ 323,400: 26x17
#233 @ 740,701: 29x11
#234 @ 486,543: 21x23
#235 @ 594,955: 19x19
#236 @ 447,474: 27x24
#237 @ 119,738: 28x29
#238 @ 32,925: 18x24
#239 @ 393,558: 29x15
#240 @ 466,334: 22x25
#241 @ 780,835: 21x26
#242 @ 276,31: 15x18
#243 @ 356,588: 20x11
#244 @ 745,641: 4x3
#245 @ 500,914: 28x11
#246 @ 962,268: 22x29
#247 @ 671,928: 12x24
#248 @ 655,743: 18x18
#249 @ 957,935: 28x22
#250 @ 907,37: 21x10
#251 @ 861,279: 19x24
#252 @ 634,591: 20x22
#253 @ 364,394: 29x21
#254 @ 609,284: 22x20
#255 @ 395,177: 15x13
#256 @ 201,26: 14x15
#257 @ 654,580: 25x13
#258 @ 152,301: 11x25
#259 @ 501,774: 18x12
#260 @ 88,936: 25x26
#261 @ 923,781: 14x19
#262 @ 568,629: 15x17
#263 @ 418,972: 26x12
#264 @ 645,345: 20x16
#265 @ 41,218: 12x24
#266 @ 597,340: 27x26
#267 @ 200,808: 25x13
#268 @ 28,2: 13x16
#269 @ 135,474: 27x22
#270 @ 34,953: 21x23
#271 @ 399,143: 12x26
#272 @ 478,599: 21x21
#273 @ 398,941: 13x19
#274 @ 532,105: 7x10
#275 @ 414,979: 11x10
#276 @ 521,756: 14x15
#277 @ 552,140: 18x16
#278 @ 536,285: 18x15
#279 @ 300,315: 28x22
#280 @ 127,129: 18x4
#281 @ 22,105: 10x23
#282 @ 817,49: 21x19
#283 @ 2,0: 29x23
#284 @ 846,613: 25x25
#285 @ 683,781: 28x13
#286 @ 282,403: 29x22
#287 @ 337,152: 15x19
#288 @ 581,759: 24x20
#289 @ 180,793: 15x14
#290 @ 58,714: 17x29
#291 @ 460,892: 25x20
#292 @ 263,104: 27x25
#293 @ 776,885: 17x14
#294 @ 605,754: 18x24
#295 @ 20,653: 12x29
#296 @ 465,975: 13x12
#297 @ 403,226: 25x16
#298 @ 362,327: 17x20
#299 @ 977,610: 21x14
#300 @ 933,656: 26x18
#301 @ 819,782: 16x18
#302 @ 126,123: 11x15
#303 @ 48,205: 13x11
#304 @ 568,564: 18x23
#305 @ 934,210: 11x11
#306 @ 780,401: 12x24
#307 @ 17,515: 16x15
#308 @ 866,14: 23x11
#309 @ 279,503: 17x11
#310 @ 574,209: 27x19
#311 @ 831,215: 26x27
#312 @ 347,431: 21x28
#313 @ 764,419: 24x25
#314 @ 877,54: 28x13
#315 @ 257,964: 10x22
#316 @ 619,148: 16x12
#317 @ 643,723: 25x16
#318 @ 844,182: 22x22
#319 @ 797,119: 29x19
#320 @ 237,767: 25x18
#321 @ 304,579: 12x12
#322 @ 158,101: 18x22
#323 @ 958,873: 15x11
#324 @ 563,621: 3x14
#325 @ 11,404: 10x19
#326 @ 719,508: 12x19
#327 @ 603,80: 18x29
#328 @ 201,544: 28x21
#329 @ 548,119: 19x12
#330 @ 646,251: 25x27
#331 @ 702,685: 22x14
#332 @ 609,601: 14x24
#333 @ 432,298: 22x28
#334 @ 954,182: 27x11
#335 @ 171,447: 13x26
#336 @ 564,807: 12x19
#337 @ 542,866: 16x24
#338 @ 733,945: 24x29
#339 @ 60,177: 21x27
#340 @ 643,732: 10x17
#341 @ 470,344: 12x25
#342 @ 791,364: 10x27
#343 @ 610,476: 11x25
#344 @ 307,203: 10x24
#345 @ 628,847: 20x15
#346 @ 441,868: 23x25
#347 @ 875,195: 16x27
#348 @ 643,151: 14x28
#349 @ 544,750: 29x29
#350 @ 462,563: 19x20
#351 @ 97,864: 15x28
#352 @ 59,668: 25x13
#353 @ 537,489: 29x24
#354 @ 116,582: 24x13
#355 @ 591,243: 18x26
#356 @ 304,642: 11x15
#357 @ 214,558: 14x25
#358 @ 133,560: 22x26
#359 @ 304,608: 28x11
#360 @ 362,864: 16x23
#361 @ 358,693: 15x27
#362 @ 859,254: 28x28
#363 @ 311,445: 11x8
#364 @ 547,289: 27x28
#365 @ 134,929: 15x15
#366 @ 613,818: 21x13
#367 @ 948,88: 28x19
#368 @ 303,241: 11x11
#369 @ 590,43: 20x25
#370 @ 975,629: 23x17
#371 @ 131,579: 28x10
#372 @ 163,780: 24x22
#373 @ 326,531: 27x27
#374 @ 328,437: 14x23
#375 @ 786,838: 17x17
#376 @ 688,104: 25x20
#377 @ 245,739: 28x10
#378 @ 425,197: 20x12
#379 @ 133,610: 14x10
#380 @ 845,3: 15x29
#381 @ 695,368: 10x25
#382 @ 612,815: 24x21
#383 @ 970,83: 20x27
#384 @ 883,829: 18x28
#385 @ 219,944: 3x9
#386 @ 101,500: 12x22
#387 @ 355,981: 16x13
#388 @ 228,952: 17x23
#389 @ 750,969: 10x23
#390 @ 475,701: 29x12
#391 @ 925,351: 23x27
#392 @ 734,87: 10x29
#393 @ 515,688: 14x24
#394 @ 696,665: 21x11
#395 @ 18,115: 15x29
#396 @ 37,347: 14x23
#397 @ 290,184: 16x23
#398 @ 894,40: 21x27
#399 @ 294,293: 29x28
#400 @ 490,648: 10x21
#401 @ 290,611: 23x23
#402 @ 597,641: 13x24
#403 @ 353,948: 25x19
#404 @ 369,167: 23x24
#405 @ 65,388: 10x21
#406 @ 718,387: 9x8
#407 @ 853,648: 28x13
#408 @ 598,77: 14x27
#409 @ 19,217: 25x14
#410 @ 455,337: 23x14
#411 @ 939,353: 11x24
#412 @ 159,264: 11x28
#413 @ 47,618: 12x12
#414 @ 656,393: 28x20
#415 @ 605,536: 28x26
#416 @ 215,549: 22x24
#417 @ 409,402: 10x15
#418 @ 707,781: 13x11
#419 @ 569,388: 23x18
#420 @ 234,394: 16x26
#421 @ 42,664: 26x10
#422 @ 382,536: 22x26
#423 @ 835,514: 10x22
#424 @ 109,567: 19x26
#425 @ 444,186: 20x29
#426 @ 707,767: 13x26
#427 @ 415,156: 26x29
#428 @ 173,75: 24x24
#429 @ 452,889: 27x19
#430 @ 134,352: 28x15
#431 @ 936,879: 26x26
#432 @ 191,11: 11x17
#433 @ 569,692: 25x23
#434 @ 115,118: 18x29
#435 @ 82,846: 19x14
#436 @ 13,283: 21x29
#437 @ 870,735: 27x19
#438 @ 40,470: 10x17
#439 @ 686,17: 21x26
#440 @ 461,555: 13x24
#441 @ 582,895: 28x29
#442 @ 159,10: 15x19
#443 @ 78,709: 22x17
#444 @ 571,939: 10x26
#445 @ 971,833: 22x22
#446 @ 820,39: 26x14
#447 @ 91,104: 14x19
#448 @ 484,245: 27x19
#449 @ 770,406: 19x27
#450 @ 233,573: 20x15
#451 @ 480,866: 24x12
#452 @ 323,941: 24x19
#453 @ 335,892: 13x21
#454 @ 572,822: 22x21
#455 @ 729,922: 29x15
#456 @ 7,661: 28x10
#457 @ 401,597: 21x19
#458 @ 211,750: 25x24
#459 @ 117,629: 23x17
#460 @ 117,116: 25x22
#461 @ 834,656: 10x12
#462 @ 367,183: 14x17
#463 @ 675,635: 23x21
#464 @ 229,828: 24x25
#465 @ 539,496: 21x9
#466 @ 221,89: 24x24
#467 @ 237,102: 24x26
#468 @ 721,901: 13x27
#469 @ 441,849: 25x25
#470 @ 254,471: 17x13
#471 @ 767,912: 23x21
#472 @ 538,167: 15x18
#473 @ 663,947: 20x17
#474 @ 605,634: 19x12
#475 @ 320,600: 27x10
#476 @ 814,141: 10x25
#477 @ 573,714: 21x26
#478 @ 591,552: 22x16
#479 @ 959,76: 12x21
#480 @ 737,950: 10x13
#481 @ 587,465: 26x21
#482 @ 25,770: 20x12
#483 @ 542,210: 13x28
#484 @ 323,508: 15x18
#485 @ 61,776: 20x20
#486 @ 17,394: 22x11
#487 @ 652,873: 11x26
#488 @ 293,939: 10x24
#489 @ 549,202: 17x15
#490 @ 973,96: 20x27
#491 @ 950,829: 23x27
#492 @ 472,538: 11x14
#493 @ 464,333: 18x25
#494 @ 12,102: 18x29
#495 @ 812,614: 27x29
#496 @ 229,285: 18x11
#497 @ 465,444: 14x19
#498 @ 643,188: 24x11
#499 @ 555,258: 17x20
#500 @ 527,431: 20x17
#501 @ 964,150: 20x21
#502 @ 572,567: 11x17
#503 @ 560,468: 10x8
#504 @ 207,553: 22x17
#505 @ 898,143: 11x24
#506 @ 438,62: 10x16
#507 @ 579,371: 13x22
#508 @ 965,618: 14x25
#509 @ 816,391: 19x23
#510 @ 467,329: 21x19
#511 @ 179,774: 26x20
#512 @ 583,502: 25x14
#513 @ 902,294: 19x25
#514 @ 634,406: 29x13
#515 @ 478,696: 11x25
#516 @ 434,57: 19x27
#517 @ 473,835: 11x15
#518 @ 750,39: 14x25
#519 @ 562,275: 12x17
#520 @ 952,415: 25x26
#521 @ 621,415: 10x21
#522 @ 742,397: 23x15
#523 @ 169,288: 10x22
#524 @ 466,436: 23x14
#525 @ 532,757: 11x22
#526 @ 81,920: 13x21
#527 @ 600,922: 12x26
#528 @ 705,728: 18x12
#529 @ 828,568: 17x9
#530 @ 267,335: 27x11
#531 @ 265,581: 14x23
#532 @ 779,109: 13x12
#533 @ 858,599: 28x16
#534 @ 646,168: 24x21
#535 @ 647,903: 17x17
#536 @ 149,530: 26x15
#537 @ 941,457: 18x18
#538 @ 631,638: 26x14
#539 @ 15,676: 20x23
#540 @ 432,598: 12x26
#541 @ 687,291: 18x11
#542 @ 264,788: 23x16
#543 @ 779,36: 13x18
#544 @ 278,18: 16x11
#545 @ 158,316: 20x14
#546 @ 808,467: 20x28
#547 @ 406,906: 21x25
#548 @ 152,111: 20x16
#549 @ 167,453: 22x11
#550 @ 477,891: 13x26
#551 @ 788,871: 29x11
#552 @ 318,485: 21x18
#553 @ 826,627: 25x12
#554 @ 160,160: 10x29
#555 @ 532,741: 19x14
#556 @ 91,571: 26x21
#557 @ 189,623: 17x16
#558 @ 427,766: 26x29
#559 @ 334,50: 13x17
#560 @ 945,9: 24x27
#561 @ 529,973: 28x10
#562 @ 384,611: 24x14
#563 @ 444,486: 12x11
#564 @ 748,903: 25x12
#565 @ 659,385: 26x26
#566 @ 536,857: 23x11
#567 @ 931,450: 13x24
#568 @ 663,373: 23x28
#569 @ 903,449: 21x13
#570 @ 35,11: 17x11
#571 @ 784,714: 28x20
#572 @ 664,589: 25x22
#573 @ 868,879: 26x20
#574 @ 949,624: 29x16
#575 @ 481,223: 13x27
#576 @ 292,641: 18x10
#577 @ 177,763: 15x16
#578 @ 74,682: 11x20
#579 @ 516,190: 28x24
#580 @ 98,218: 29x25
#581 @ 707,338: 22x16
#582 @ 309,440: 18x17
#583 @ 364,574: 26x28
#584 @ 127,727: 17x23
#585 @ 918,821: 11x17
#586 @ 184,1: 20x28
#587 @ 645,377: 22x14
#588 @ 274,632: 28x14
#589 @ 757,624: 22x10
#590 @ 517,14: 22x22
#591 @ 326,278: 19x13
#592 @ 752,469: 12x27
#593 @ 396,539: 13x21
#594 @ 313,323: 28x27
#595 @ 799,21: 16x19
#596 @ 635,85: 29x19
#597 @ 501,596: 16x21
#598 @ 346,843: 11x21
#599 @ 330,941: 28x26
#600 @ 338,95: 17x22
#601 @ 783,373: 24x12
#602 @ 884,915: 23x11
#603 @ 235,426: 29x22
#604 @ 108,687: 21x27
#605 @ 67,576: 23x26
#606 @ 42,474: 3x6
#607 @ 332,145: 26x14
#608 @ 235,918: 16x25
#609 @ 124,941: 21x14
#610 @ 658,688: 25x14
#611 @ 817,146: 10x18
#612 @ 533,495: 27x22
#613 @ 654,946: 19x28
#614 @ 253,258: 10x23
#615 @ 771,703: 11x24
#616 @ 17,960: 20x14
#617 @ 187,27: 18x21
#618 @ 179,447: 19x26
#619 @ 161,150: 24x20
#620 @ 894,459: 25x24
#621 @ 983,649: 14x15
#622 @ 453,291: 28x25
#623 @ 20,545: 29x13
#624 @ 486,439: 21x13
#625 @ 343,352: 19x18
#626 @ 848,912: 26x11
#627 @ 535,720: 11x19
#628 @ 135,168: 15x26
#629 @ 449,623: 18x12
#630 @ 596,288: 24x20
#631 @ 905,829: 18x28
#632 @ 953,941: 14x11
#633 @ 951,871: 28x11
#634 @ 559,379: 10x23
#635 @ 757,468: 10x15
#636 @ 132,584: 18x23
#637 @ 591,81: 26x16
#638 @ 31,623: 29x15
#639 @ 699,335: 15x15
#640 @ 747,32: 25x22
#641 @ 560,450: 17x15
#642 @ 870,293: 14x16
#643 @ 106,92: 18x15
#644 @ 173,315: 12x29
#645 @ 769,102: 23x15
#646 @ 842,912: 27x20
#647 @ 715,335: 23x11
#648 @ 744,455: 13x11
#649 @ 322,101: 15x14
#650 @ 617,583: 24x25
#651 @ 752,350: 12x19
#652 @ 541,881: 11x23
#653 @ 873,817: 24x22
#654 @ 215,318: 15x28
#655 @ 904,233: 19x18
#656 @ 769,115: 23x22
#657 @ 499,169: 25x26
#658 @ 332,622: 20x19
#659 @ 86,766: 10x19
#660 @ 67,616: 28x23
#661 @ 338,186: 23x18
#662 @ 765,306: 15x26
#663 @ 536,963: 11x20
#664 @ 161,160: 10x16
#665 @ 185,148: 26x13
#666 @ 472,212: 10x25
#667 @ 638,509: 13x17
#668 @ 932,415: 21x14
#669 @ 240,956: 18x29
#670 @ 705,2: 27x11
#671 @ 113,722: 21x17
#672 @ 543,379: 19x20
#673 @ 696,312: 15x26
#674 @ 954,102: 16x16
#675 @ 526,781: 24x24
#676 @ 969,729: 24x15
#677 @ 187,129: 17x28
#678 @ 204,244: 25x10
#679 @ 93,745: 10x15
#680 @ 16,930: 27x17
#681 @ 50,205: 29x29
#682 @ 23,958: 25x28
#683 @ 727,841: 10x20
#684 @ 652,914: 17x22
#685 @ 125,120: 23x21
#686 @ 917,762: 21x26
#687 @ 220,20: 29x16
#688 @ 653,883: 18x12
#689 @ 427,962: 19x15
#690 @ 192,559: 23x10
#691 @ 889,93: 17x25
#692 @ 943,25: 22x25
#693 @ 782,854: 21x16
#694 @ 194,557: 18x16
#695 @ 931,928: 21x23
#696 @ 347,855: 15x19
#697 @ 74,554: 20x10
#698 @ 487,199: 13x27
#699 @ 777,815: 11x18
#700 @ 710,571: 20x24
#701 @ 805,28: 13x20
#702 @ 362,579: 24x22
#703 @ 99,848: 21x19
#704 @ 706,2: 23x15
#705 @ 349,896: 29x13
#706 @ 795,321: 24x12
#707 @ 0,978: 12x17
#708 @ 767,140: 12x11
#709 @ 350,266: 19x28
#710 @ 721,586: 12x22
#711 @ 547,392: 27x10
#712 @ 870,465: 25x18
#713 @ 556,809: 15x16
#714 @ 84,223: 17x18
#715 @ 704,156: 12x23
#716 @ 793,356: 14x23
#717 @ 185,672: 26x24
#718 @ 250,406: 28x19
#719 @ 301,210: 12x15
#720 @ 72,160: 22x22
#721 @ 180,853: 11x27
#722 @ 794,608: 21x21
#723 @ 850,392: 26x23
#724 @ 351,152: 21x20
#725 @ 697,608: 14x12
#726 @ 477,242: 19x23
#727 @ 208,721: 28x27
#728 @ 83,510: 24x26
#729 @ 626,834: 15x14
#730 @ 12,647: 27x24
#731 @ 695,1: 11x13
#732 @ 466,691: 16x13
#733 @ 916,250: 27x19
#734 @ 152,461: 29x10
#735 @ 683,832: 11x15
#736 @ 503,913: 27x27
#737 @ 230,84: 20x25
#738 @ 705,573: 24x10
#739 @ 641,668: 13x29
#740 @ 594,509: 10x15
#741 @ 274,972: 27x25
#742 @ 571,61: 27x20
#743 @ 280,311: 16x25
#744 @ 559,266: 27x14
#745 @ 474,682: 22x23
#746 @ 668,365: 29x21
#747 @ 450,345: 18x26
#748 @ 778,601: 29x10
#749 @ 353,151: 17x16
#750 @ 325,539: 5x7
#751 @ 250,233: 25x25
#752 @ 420,349: 29x17
#753 @ 5,218: 29x29
#754 @ 651,743: 19x13
#755 @ 405,331: 13x22
#756 @ 858,390: 21x14
#757 @ 284,424: 22x25
#758 @ 757,917: 19x22
#759 @ 730,911: 16x24
#760 @ 688,363: 10x15
#761 @ 721,585: 29x11
#762 @ 14,295: 17x23
#763 @ 720,588: 13x14
#764 @ 213,610: 29x15
#765 @ 816,27: 11x27
#766 @ 864,2: 22x13
#767 @ 630,666: 23x20
#768 @ 328,106: 12x29
#769 @ 595,159: 10x17
#770 @ 937,888: 27x10
#771 @ 649,377: 29x24
#772 @ 199,83: 28x27
#773 @ 242,962: 20x22
#774 @ 713,568: 11x25
#775 @ 303,188: 25x20
#776 @ 2,158: 16x29
#777 @ 168,902: 21x20
#778 @ 138,241: 25x11
#779 @ 936,78: 6x3
#780 @ 798,526: 10x16
#781 @ 159,154: 12x24
#782 @ 425,267: 13x25
#783 @ 686,157: 21x26
#784 @ 62,320: 21x27
#785 @ 392,973: 17x26
#786 @ 502,346: 17x13
#787 @ 763,62: 26x28
#788 @ 130,251: 24x14
#789 @ 930,201: 12x28
#790 @ 472,613: 14x29
#791 @ 525,496: 17x11
#792 @ 131,939: 12x21
#793 @ 704,296: 14x14
#794 @ 417,615: 20x13
#795 @ 340,139: 10x17
#796 @ 415,616: 10x20
#797 @ 266,33: 29x24
#798 @ 979,612: 16x8
#799 @ 486,871: 29x24
#800 @ 309,492: 13x15
#801 @ 554,771: 26x13
#802 @ 545,551: 7x4
#803 @ 314,577: 19x29
#804 @ 43,223: 10x21
#805 @ 473,609: 19x23
#806 @ 747,694: 23x28
#807 @ 369,767: 21x28
#808 @ 368,736: 27x15
#809 @ 195,602: 10x25
#810 @ 9,644: 23x13
#811 @ 342,310: 11x17
#812 @ 967,804: 19x26
#813 @ 692,110: 29x25
#814 @ 350,538: 22x11
#815 @ 552,394: 14x3
#816 @ 897,902: 18x23
#817 @ 885,592: 21x15
#818 @ 675,4: 24x19
#819 @ 969,717: 13x19
#820 @ 635,130: 27x29
#821 @ 343,678: 12x7
#822 @ 588,929: 22x27
#823 @ 748,565: 25x18
#824 @ 460,621: 13x16
#825 @ 689,535: 23x27
#826 @ 325,874: 20x27
#827 @ 314,544: 19x27
#828 @ 550,644: 12x12
#829 @ 415,260: 29x16
#830 @ 228,435: 25x12
#831 @ 379,568: 17x15
#832 @ 638,386: 27x12
#833 @ 335,293: 25x19
#834 @ 699,600: 27x21
#835 @ 534,675: 20x10
#836 @ 774,160: 20x29
#837 @ 25,208: 12x23
#838 @ 298,633: 29x25
#839 @ 293,185: 11x14
#840 @ 646,952: 19x26
#841 @ 505,912: 20x20
#842 @ 216,435: 20x28
#843 @ 824,564: 27x21
#844 @ 731,414: 27x11
#845 @ 131,716: 14x25
#846 @ 657,348: 19x22
#847 @ 115,753: 11x16
#848 @ 897,358: 22x25
#849 @ 229,580: 26x19
#850 @ 389,151: 27x24
#851 @ 168,301: 28x17
#852 @ 427,175: 28x12
#853 @ 206,319: 13x27
#854 @ 109,372: 28x27
#855 @ 849,407: 27x12
#856 @ 467,211: 21x24
#857 @ 782,522: 24x11
#858 @ 450,439: 22x10
#859 @ 502,781: 21x24
#860 @ 529,103: 14x24
#861 @ 2,975: 28x12
#862 @ 921,519: 16x21
#863 @ 738,627: 22x21
#864 @ 606,38: 12x21
#865 @ 852,336: 16x15
#866 @ 770,556: 20x10
#867 @ 317,56: 11x26
#868 @ 295,337: 12x25
#869 @ 139,925: 24x22
#870 @ 825,823: 15x24
#871 @ 302,198: 17x10
#872 @ 462,299: 23x29
#873 @ 329,452: 24x15
#874 @ 649,833: 19x12
#875 @ 3,230: 12x28
#876 @ 838,129: 27x14
#877 @ 361,159: 28x28
#878 @ 70,535: 22x12
#879 @ 611,786: 18x15
#880 @ 683,48: 25x26
#881 @ 958,141: 19x10
#882 @ 655,718: 26x22
#883 @ 442,159: 10x10
#884 @ 567,716: 28x12
#885 @ 967,174: 29x13
#886 @ 716,768: 23x24
#887 @ 866,59: 12x16
#888 @ 450,775: 13x23
#889 @ 239,605: 11x29
#890 @ 900,34: 18x22
#891 @ 472,277: 15x25
#892 @ 895,865: 20x27
#893 @ 980,926: 19x27
#894 @ 727,613: 25x29
#895 @ 7,398: 24x12
#896 @ 465,616: 18x26
#897 @ 330,510: 27x16
#898 @ 258,592: 13x16
#899 @ 417,401: 28x16
#900 @ 782,391: 27x13
#901 @ 412,963: 18x15
#902 @ 444,599: 10x18
#903 @ 55,715: 13x11
#904 @ 452,441: 15x5
#905 @ 544,294: 27x15
#906 @ 884,521: 18x29
#907 @ 623,464: 17x11
#908 @ 370,156: 29x16
#909 @ 3,846: 24x16
#910 @ 190,890: 19x13
#911 @ 552,831: 5x4
#912 @ 591,597: 25x19
#913 @ 653,338: 16x25
#914 @ 416,264: 11x29
#915 @ 273,388: 12x26
#916 @ 908,520: 20x27
#917 @ 144,523: 12x26
#918 @ 429,615: 18x19
#919 @ 615,523: 16x14
#920 @ 757,972: 24x20
#921 @ 6,222: 10x19
#922 @ 23,972: 10x16
#923 @ 706,583: 21x26
#924 @ 73,247: 25x10
#925 @ 496,762: 26x29
#926 @ 865,748: 19x13
#927 @ 659,919: 12x17
#928 @ 790,378: 27x28
#929 @ 747,442: 18x24
#930 @ 709,769: 8x20
#931 @ 184,893: 15x28
#932 @ 456,33: 28x11
#933 @ 882,271: 10x18
#934 @ 587,170: 20x17
#935 @ 867,866: 10x17
#936 @ 969,953: 14x18
#937 @ 716,384: 14x19
#938 @ 323,260: 10x23
#939 @ 224,562: 20x19
#940 @ 192,249: 25x11
#941 @ 87,775: 26x24
#942 @ 766,146: 20x17
#943 @ 219,706: 23x16
#944 @ 581,397: 16x29
#945 @ 733,958: 18x28
#946 @ 955,207: 29x15
#947 @ 906,855: 19x17
#948 @ 569,705: 27x12
#949 @ 361,588: 11x13
#950 @ 149,711: 20x20
#951 @ 73,558: 12x23
#952 @ 541,717: 27x10
#953 @ 303,570: 15x11
#954 @ 666,923: 26x27
#955 @ 25,343: 17x28
#956 @ 190,870: 26x24
#957 @ 749,99: 13x24
#958 @ 885,844: 19x26
#959 @ 371,611: 13x18
#960 @ 938,476: 23x27
#961 @ 247,761: 20x23
#962 @ 135,608: 24x19
#963 @ 558,450: 27x15
#964 @ 841,388: 22x10
#965 @ 754,474: 28x19
#966 @ 392,965: 23x28
#967 @ 24,10: 28x13
#968 @ 559,619: 11x20
#969 @ 259,455: 14x23
#970 @ 833,477: 14x12
#971 @ 667,871: 20x25
#972 @ 361,753: 27x14
#973 @ 481,599: 13x20
#974 @ 704,96: 18x25
#975 @ 48,940: 25x29
#976 @ 625,795: 10x11
#977 @ 667,576: 14x15
#978 @ 160,967: 20x23
#979 @ 168,155: 12x9
#980 @ 182,165: 26x21
#981 @ 587,916: 15x10
#982 @ 589,40: 11x22
#983 @ 676,235: 15x18
#984 @ 717,126: 13x13
#985 @ 481,834: 22x22
#986 @ 810,649: 26x13
#987 @ 658,742: 7x17
#988 @ 553,454: 24x27
#989 @ 908,422: 17x16
#990 @ 822,532: 21x17
#991 @ 364,127: 13x17
#992 @ 598,462: 27x11
#993 @ 438,749: 17x29
#994 @ 536,394: 16x25
#995 @ 718,913: 14x22
#996 @ 748,331: 14x27
#997 @ 388,745: 29x10
#998 @ 520,923: 22x13
#999 @ 915,186: 16x3
#1000 @ 293,594: 27x25
#1001 @ 342,142: 23x27
#1002 @ 859,197: 16x24
#1003 @ 483,331: 29x21
#1004 @ 816,861: 10x23
#1005 @ 664,913: 17x21
#1006 @ 482,314: 15x13
#1007 @ 347,84: 10x26
#1008 @ 140,766: 23x15
#1009 @ 768,686: 26x21
#1010 @ 588,32: 26x17
#1011 @ 913,900: 20x19
#1012 @ 581,211: 16x10
#1013 @ 642,754: 18x12
#1014 @ 678,155: 11x13
#1015 @ 804,614: 12x11
#1016 @ 233,594: 18x21
#1017 @ 83,801: 20x22
#1018 @ 136,728: 27x19
#1019 @ 117,629: 13x10
#1020 @ 167,173: 13x11
#1021 @ 244,723: 13x13
#1022 @ 890,371: 19x22
#1023 @ 741,968: 13x28
#1024 @ 485,210: 26x15
#1025 @ 316,537: 22x13
#1026 @ 211,8: 12x23
#1027 @ 885,97: 18x29
#1028 @ 240,245: 16x18
#1029 @ 914,823: 16x20
#1030 @ 704,910: 18x12
#1031 @ 247,741: 16x4
#1032 @ 131,748: 21x16
#1033 @ 593,918: 13x10
#1034 @ 305,607: 11x16
#1035 @ 646,604: 19x19
#1036 @ 792,51: 15x11
#1037 @ 900,425: 25x12
#1038 @ 884,12: 11x12
#1039 @ 502,755: 27x24
#1040 @ 434,770: 15x11
#1041 @ 897,441: 22x20
#1042 @ 847,327: 12x19
#1043 @ 137,103: 21x26
#1044 @ 961,686: 10x29
#1045 @ 366,141: 29x21
#1046 @ 96,600: 14x11
#1047 @ 696,684: 12x23
#1048 @ 110,89: 19x23
#1049 @ 482,454: 21x20
#1050 @ 542,882: 21x11
#1051 @ 237,579: 13x14
#1052 @ 912,158: 12x14
#1053 @ 917,366: 26x21
#1054 @ 887,198: 14x11
#1055 @ 708,336: 14x11
#1056 @ 702,288: 25x29
#1057 @ 756,389: 12x20
#1058 @ 928,503: 14x10
#1059 @ 851,459: 22x14
#1060 @ 721,516: 7x7
#1061 @ 232,572: 12x19
#1062 @ 670,943: 29x15
#1063 @ 767,902: 20x22
#1064 @ 686,675: 16x23
#1065 @ 850,644: 18x16
#1066 @ 905,403: 23x29
#1067 @ 237,958: 16x10
#1068 @ 457,694: 27x25
#1069 @ 18,396: 19x20
#1070 @ 355,536: 22x13
#1071 @ 47,551: 29x16
#1072 @ 229,125: 14x10
#1073 @ 702,109: 10x28
#1074 @ 653,906: 7x16
#1075 @ 889,524: 9x19
#1076 @ 817,736: 24x26
#1077 @ 121,270: 13x20
#1078 @ 706,157: 20x19
#1079 @ 205,963: 12x18
#1080 @ 474,617: 11x29
#1081 @ 646,897: 28x22
#1082 @ 308,618: 22x19
#1083 @ 781,157: 10x17
#1084 @ 751,975: 15x18
#1085 @ 181,628: 24x18
#1086 @ 710,711: 15x22
#1087 @ 115,149: 25x19
#1088 @ 406,765: 11x4
#1089 @ 923,460: 12x14
#1090 @ 407,839: 19x4
#1091 @ 549,801: 25x13
#1092 @ 778,476: 19x10
#1093 @ 234,173: 23x25
#1094 @ 110,871: 11x23
#1095 @ 914,902: 22x12
#1096 @ 953,32: 17x15
#1097 @ 792,892: 13x23
#1098 @ 137,292: 24x17
#1099 @ 459,315: 10x21
#1100 @ 913,181: 23x16
#1101 @ 832,559: 21x16
#1102 @ 979,591: 20x22
#1103 @ 763,370: 14x26
#1104 @ 799,603: 24x26
#1105 @ 263,110: 17x25
#1106 @ 659,582: 20x19
#1107 @ 799,367: 3x8
#1108 @ 545,872: 26x25
#1109 @ 329,87: 15x20
#1110 @ 908,218: 26x15
#1111 @ 804,598: 28x27
#1112 @ 75,44: 24x29
#1113 @ 568,835: 16x22
#1114 @ 636,482: 24x26
#1115 @ 300,338: 24x10
#1116 @ 884,186: 17x29
#1117 @ 787,901: 19x22
#1118 @ 424,257: 10x28
#1119 @ 268,59: 14x11
#1120 @ 382,327: 29x14
#1121 @ 445,852: 15x7
#1122 @ 765,821: 25x23
#1123 @ 780,915: 22x16
#1124 @ 128,709: 10x13
#1125 @ 372,853: 28x12
#1126 @ 759,106: 18x27
#1127 @ 714,469: 13x28
#1128 @ 26,830: 16x27
#1129 @ 282,958: 28x13
#1130 @ 662,936: 20x24
#1131 @ 114,381: 8x10
#1132 @ 319,102: 24x25
#1133 @ 361,599: 27x27
#1134 @ 322,501: 14x13
#1135 @ 617,420: 11x10
#1136 @ 931,920: 25x23
#1137 @ 185,779: 16x28
#1138 @ 732,581: 23x16
#1139 @ 196,794: 15x11
#1140 @ 9,516: 21x25
#1141 @ 233,521: 28x20
#1142 @ 651,897: 12x29
#1143 @ 166,122: 25x20
#1144 @ 755,388: 4x10
#1145 @ 222,881: 27x29
#1146 @ 786,851: 11x11
#1147 @ 983,541: 16x28
#1148 @ 102,118: 14x11
#1149 @ 202,503: 19x21
#1150 @ 74,812: 15x18
#1151 @ 545,94: 25x28
#1152 @ 902,335: 19x25
#1153 @ 803,602: 11x11
#1154 @ 137,112: 14x17
#1155 @ 458,468: 11x23
#1156 @ 340,323: 21x29
#1157 @ 340,611: 22x28
#1158 @ 833,831: 13x12
#1159 @ 257,104: 12x15
#1160 @ 828,599: 21x19
#1161 @ 279,379: 24x14
#1162 @ 528,751: 14x14
#1163 @ 928,207: 11x23
#1164 @ 236,955: 20x14
#1165 @ 368,823: 20x26
#1166 @ 329,411: 26x15
#1167 @ 160,747: 20x25
#1168 @ 753,385: 10x17
#1169 @ 989,574: 11x29
#1170 @ 644,516: 26x12
#1171 @ 704,751: 13x24
#1172 @ 392,282: 14x10
#1173 @ 295,927: 22x29
#1174 @ 862,191: 24x26
#1175 @ 889,38: 18x14
#1176 @ 844,341: 17x21
#1177 @ 919,661: 17x24
#1178 @ 289,928: 23x16
#1179 @ 325,440: 10x11
#1180 @ 581,574: 21x28
#1181 @ 41,212: 23x11
#1182 @ 235,825: 25x14
#1183 @ 482,432: 19x22
#1184 @ 954,488: 28x11
#1185 @ 442,148: 25x18
#1186 @ 428,585: 16x20
#1187 @ 706,491: 19x26
#1188 @ 637,44: 15x11
#1189 @ 474,957: 19x24
#1190 @ 184,846: 18x27
#1191 @ 889,16: 21x28
#1192 @ 865,726: 16x24
#1193 @ 321,579: 27x14
#1194 @ 82,531: 22x27
#1195 @ 961,733: 17x21
#1196 @ 368,137: 10x20
#1197 @ 494,441: 25x22
#1198 @ 859,311: 20x17
#1199 @ 469,707: 24x28
#1200 @ 161,404: 16x29
#1201 @ 798,307: 17x16
#1202 @ 408,137: 10x10
#1203 @ 19,817: 14x22
#1204 @ 490,638: 18x17
#1205 @ 651,96: 23x25
#1206 @ 69,694: 20x24
#1207 @ 270,617: 13x20
#1208 @ 239,878: 29x10
#1209 @ 961,568: 25x12
#1210 @ 75,34: 18x23
#1211 @ 961,707: 28x28
#1212 @ 969,939: 16x27
#1213 @ 280,35: 24x21
#1214 @ 710,915: 11x20
#1215 @ 670,625: 28x27
#1216 @ 109,872: 21x15
#1217 @ 588,735: 13x29
#1218 @ 175,178: 20x12
#1219 @ 691,718: 25x15
#1220 @ 2,157: 11x10
#1221 @ 603,529: 10x28
#1222 @ 0,296: 19x15
#1223 @ 576,962: 10x15
#1224 @ 640,141: 24x21
#1225 @ 760,295: 18x21
#1226 @ 699,416: 24x21
#1227 @ 313,520: 11x10
#1228 @ 736,791: 21x21
#1229 @ 973,487: 14x28
#1230 @ 473,359: 24x14
#1231 @ 230,68: 11x22
#1232 @ 116,865: 28x25
#1233 @ 136,568: 25x27
#1234 @ 546,101: 27x24
#1235 @ 625,664: 10x27
#1236 @ 377,112: 11x29
#1237 @ 770,707: 18x12
#1238 @ 856,913: 27x24
#1239 @ 976,648: 16x28
#1240 @ 579,757: 10x22
#1241 @ 723,564: 19x23
#1242 @ 729,67: 25x27
#1243 @ 436,274: 16x21
#1244 @ 831,2: 28x20
#1245 @ 293,14: 26x10
#1246 @ 77,898: 19x29
#1247 @ 147,667: 10x29
#1248 @ 585,760: 16x26
#1249 @ 110,695: 17x22
#1250 @ 935,343: 19x23
#1251 @ 742,900: 25x20
#1252 @ 643,681: 10x18
#1253 @ 634,655: 11x23
#1254 @ 103,513: 11x22
#1255 @ 827,743: 14x14
#1256 @ 382,774: 23x14
#1257 @ 738,793: 13x5
#1258 @ 972,268: 26x22
#1259 @ 237,242: 12x27
#1260 @ 521,701: 11x20
#1261 @ 936,642: 13x25
#1262 @ 68,314: 11x13
#1263 @ 342,752: 23x21
#1264 @ 590,929: 20x25
#1265 @ 3,703: 18x29
#1266 @ 654,877: 29x16
#1267 @ 950,470: 26x12
#1268 @ 179,51: 13x24
#1269 @ 668,829: 23x24
#1270 @ 270,788: 20x11
#1271 @ 596,538: 15x12
#1272 @ 239,917: 20x23
#1273 @ 240,283: 15x13
#1274 @ 76,306: 17x21
#1275 @ 604,239: 17x10
#1276 @ 733,389: 10x13
#1277 @ 937,358: 12x11
#1278 @ 438,886: 19x27
#1279 @ 121,784: 18x23
#1280 @ 575,422: 20x16
#1281 @ 521,704: 18x17
#1282 @ 691,419: 21x11
#1283 @ 66,468: 22x22
#1284 @ 530,103: 12x23
#1285 @ 752,104: 25x29
#1286 @ 899,62: 26x13
#1287 @ 288,248: 23x25
#1288 @ 353,196: 16x16
#1289 @ 920,194: 11x23
#1290 @ 76,21: 22x25
#1291 @ 866,56: 17x12
#1292 @ 322,337: 24x17
#1293 @ 892,380: 15x24
#1294 @ 945,355: 20x14
#1295 @ 25,634: 18x20
#1296 @ 68,786: 16x18
#1297 @ 231,948: 10x19
#1298 @ 337,310: 25x14
#1299 @ 473,907: 22x14
#1300 @ 714,118: 19x26
#1301 @ 132,591: 11x20
#1302 @ 849,375: 14x16
#1303 @ 212,942: 22x15
#1304 @ 168,539: 21x25
#1305 @ 599,368: 27x26
#1306 @ 316,74: 14x25
#1307 @ 636,641: 14x3
#1308 @ 967,961: 28x12
#1309 @ 664,202: 21x23
#1310 @ 864,732: 20x14
#1311 @ 525,743: 21x27
#1312 @ 913,170: 13x15
#1313 @ 196,810: 13x27
#1314 @ 121,539: 26x17
#1315 @ 301,586: 22x15
#1316 @ 387,776: 7x9
#1317 @ 888,148: 20x16
#1318 @ 68,314: 16x13
#1319 @ 627,650: 10x21
#1320 @ 481,365: 22x11
#1321 @ 765,928: 10x13
#1322 @ 149,715: 16x14
#1323 @ 807,141: 25x27
#1324 @ 26,251: 14x18
#1325 @ 735,605: 15x12
#1326 @ 330,154: 26x12
#1327 @ 505,778: 21x18
#1328 @ 59,384: 19x24
#1329 @ 318,608: 10x22
#1330 @ 401,592: 19x22
#1331 @ 661,730: 11x18
#1332 @ 664,242: 21x27
#1333 @ 84,238: 23x27
#1334 @ 552,270: 10x14
#1335 @ 196,900: 20x13
#1336 @ 916,193: 22x27
#1337 @ 541,155: 20x21
#1338 @ 925,820: 19x19
#1339 @ 402,611: 11x25
#1340 @ 611,736: 22x24
#1341 @ 505,89: 27x25
#1342 @ 246,732: 10x11
#1343 @ 218,622: 12x19
#1344 @ 457,147: 15x25
#1345 @ 785,505: 25x17
#1346 @ 470,565: 29x28
#1347 @ 227,175: 25x27
#1348 @ 117,89: 12x11
#1349 @ 860,459: 22x15
#1350 @ 227,446: 19x13
#1351 @ 161,421: 17x19
#1352 @ 71,480: 27x12
#1353 @ 117,796: 14x10
#1354 @ 523,434: 10x15
#1355 @ 587,699: 12x16
#1356 @ 643,601: 16x28
#1357 @ 73,607: 24x23
#1358 @ 908,284: 29x16
#1359 @ 931,653: 21x27
#1360 @ 500,158: 25x24
#1361 @ 130,712: 5x5
#1362 @ 506,263: 20x22
#1363 @ 631,475: 29x19
#1364 @ 434,768: 21x14
#1365 @ 824,458: 28x17"""
