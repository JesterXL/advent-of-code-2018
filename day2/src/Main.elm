{- 

    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden

    https://adventofcode.com/2018/day/2

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


type alias Model =
    { checksumText : String -- big ole string full of checksums
    , characterCount : Int -- how many 2's and 3's are there multipled together
    , matchedChecksumCharacters : String
    }



-- which checksum strings match up to 25 characters

initialModel : Model
initialModel =
    { checksumText = ""
    , characterCount = 0
    , matchedChecksumCharacters = ""
    }


type Msg
    = InputChecksumsText String -- when you type or paste in the text area
    | ParseChecksumsText -- RUN THE MAGIC
    | LoadFromCache -- call the checksums function to load from the code vs. copy pasta


type alias CharacterMatch =
    { char2 : Int
    , char3 : Int
    }

-- [Challenge 1 Functions] ------------------------------------------------------------

calculateChecksum : CharacterMatch -> Int
calculateChecksum characterMatch =
    characterMatch.char2 * characterMatch.char3



-- because I don't know how to do partialRight in Elm

setInsertRight : Set Char -> Char -> Set Char
setInsertRight set value =
    Set.insert value set



-- Maybe's are such a pain, lol, I remember when I thought they were awesome

insertWithDefault : List Char -> Set Char -> Set Char
insertWithDefault listWithLetters letterSet =
    List.head listWithLetters
        |> Maybe.withDefault '?'
        |> setInsertRight letterSet



-- List/Array partition will give you the matches in the first part of the Array
-- so I snag out the first part and throw away the 2nd
-- ex: (['a', 'a'], ['b', 'c', 'd'])

partitionSameLetters charList letter =
    partition (\char -> char == letter) charList
        |> Tuple.first



-- all characters that match will be put into a single Array
-- you'll get things like ['a', 'a'] and ['i', 'i', 'i']

filterOutSameLetters charList =
    List.map (partitionSameLetters charList) charList
        |> List.filter (\matchList -> List.length matchList > 1)



-- List.head, you so cray with yo May-Bae!
-- seriously, it's too hard to play so I default, eh?
getFirstLetterOrNoClue : List Char -> Char
getFirstLetterOrNoClue list =
    Maybe.withDefault '?' (List.head list)


-- Dictionaries are cool in that if the value doesn't exist,
-- it'll set it. However, it's a bit cumbersome to increment
-- those values so this helps me do that. Now I can count
-- how many times I saw a particular letter (['a', 'a'] would be 2, ['d'] would be 1)
incrementDictValue : Dict Char Int -> Char -> Dict Char Int
incrementDictValue dict key =
    let
        currentValue =
            Maybe.withDefault 0 (Dict.get key dict)
    in
    Dict.update key (\maybeVal -> Just (currentValue + 1)) dict


count2And3CharactersInChecksum : List Char -> CharacterMatch
count2And3CharactersInChecksum checksumCharList =
    let
        filterOutSameLettersResult =
            filterOutSameLetters checksumCharList

        -- combine all the Array's to a Dictionary to count the occurences
        combinedDict =
            List.foldl (\matchList foundDict -> incrementDictValue foundDict (getFirstLetterOrNoClue matchList)) Dict.empty filterOutSameLettersResult

        -- filter out the 2's and 3's, and only count once, even if found multiple times. The 4 threw me for a loop, heh!
        datMatches =
            Dict.foldl
                (\charKey charCount charMatch ->
                    if charCount == 2 then
                        { charMatch | char2 = 1 }

                    else if charCount == 3 then
                        { charMatch | char3 = 1 }
                        -- Leaving this out because it has to be EXACTLY twice or EXACTLY three times, not 4 which could be wrongly interpretted as twice
                        -- else if charCount == 4 then
                        --     { charMatch | char2 = 1}

                    else
                        let
                            -- wat = log "not 2 or 3 or 4" charCount
                            wat =
                                "not logging right meow"
                        in
                        charMatch
                )
                (CharacterMatch 0 0)
                combinedDict
    in
    datMatches


-- Take all those CharacterMatch's, and just merge into 1, then multiple 2 and 3, and you done, playa!
combineCharacterMatches : List CharacterMatch -> CharacterMatch
combineCharacterMatches characterMatches =
    List.foldl (\match acc -> { acc | char2 = acc.char2 + match.char2, char3 = acc.char3 + match.char3 }) (CharacterMatch 0 0) characterMatches

-- [Challenge 2 Functions] ------------------------------------------------------------

-- A character and if it matched or not
type alias CharsMatch =
    { char : Char
    , match : Bool
    }

-- result from comparing 2 Array's of characters
-- the only part that is super important is matchCount; if that == 25, we done! 26 is the size of a checksum.
type alias ChecksumAlmostMatch =
    { firstArray : Array Char
    , secondArray : Array Char
    , empty : Bool
    , matchCount : Int
    , matchedCharacters : String
    }

-- this took like 2 days. I was using Lists, then converted to Array so I could use the get keyword
-- My fold-fu just ain't there yet.
-- This'll compare 2 Array's full of characters, and let you know which ones matched, and how many.
compare2ArraysChars firstArray secondArray =
    let
        -- Compare 2 characters, and store if they matched or not.
        matches =
            Array.indexedMap
                (\charIndex _ ->
                    let
                        firstChar =
                            Maybe.withDefault '?' (Array.get charIndex firstArray)

                        secondChar =
                            Maybe.withDefault '?' (Array.get charIndex secondArray)

                        charsMatch =
                            firstChar == secondChar
                    in
                    CharsMatch firstChar charsMatch
                )
                firstArray

        -- next, only keep the matches
        filteredMatches =
            Array.filter (\item -> item.match == True) matches

        -- count how many matches we found (we're looking for 25; 26 means we compared against the same checksum, false positive)
        matchCount =
            Array.length filteredMatches

        -- get all the characters that matched; if one is 25, we'll use these for our answer
        matchedCharacters =
            Array.map (\charsMatch -> charsMatch.char) filteredMatches
                |> Array.toList
                |> String.fromList
    in
    if matchCount == checksumLength - 1 then
        ChecksumAlmostMatch firstArray secondArray False matchCount matchedCharacters

    else if matchCount == checksumLength then -- take out exact matches
        ChecksumAlmostMatch Array.empty Array.empty True matchCount matchedCharacters

    else
        ChecksumAlmostMatch Array.empty Array.empty True matchCount matchedCharacters


-- uses the above vs. recursion and takes awhile because you're comparing 2 Array's against all elements
compare2Arrays charList arrayOfCharLists =
    let
        matches =
            Array.map (compare2ArraysChars charList) arrayOfCharLists

        nonEmptyMatches =
            Array.filter (\match -> match.empty == False) matches
    in
    nonEmptyMatches


-- split the checksum's by newline,
-- convert the String to a list of characters,
-- convert to Array's since they are easier to get a particular index on.
parseChecksums : Array (Array Char)
parseChecksums =
    split "\n" checksums
        |> List.map toList
        |> List.map Array.fromList
        |> Array.fromList


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- when you type or copy pasta into the text area
        InputChecksumsText text ->
            { model | checksumText = text }

        -- put the checksums function text into the text area
        LoadFromCache ->
            { model | checksumText = checksums }

        -- do the magic
        ParseChecksumsText ->
            let
                -- Challenge 1
                -- Attempt #1: The checksum 85272 is too damn high!
                -- Attempt #2: The checksum 646 is too low, like my self-esteem in this language.
                -- Attempt #3: 647 is too low
                -- Attempt #4: 14234 is not correct
                -- Attempt #5: 5412 is not correct
                -- Attempt #6: OH YEAH BABY! 5390

                charTotal =
                    -- List.take 10 (split "\n" checksums)
                    split "\n" checksums
                        |> List.map toList
                        -- |> List.map sortCharsAlphabetical
                        |> List.map count2And3CharactersInChecksum
                        |> combineCharacterMatches
                        |> calculateChecksum

                -- Challenge 2
                -- Attemp #1: "nvosmkcdtdbfhyxsphzgraljq", OH YEAH, FIRST TRY, WHAT'S UP WHAT'S UP
                originalChecksums =
                    parseChecksums

                allMatches =
                    Array.map
                        (\currentCharList ->
                            compare2Arrays currentCharList originalChecksums
                        )
                        parseChecksums

                blankChecksumAlmostMatch =
                    ChecksumAlmostMatch (Array.fromList [ '?' ]) (Array.fromList [ '?' ]) True 0 "?"

                filteredResults =
                    Array.filter (\possibleMatch -> Array.length possibleMatch > 0) allMatches
                        |> Array.get 0
                        |> Maybe.withDefault Array.empty
                        |> Array.get 0
                        |> Maybe.withDefault blankChecksumAlmostMatch
            in
            { model | characterCount = charTotal, matchedChecksumCharacters = filteredResults.matchedCharacters }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "demo-card-wide mdl-card mdl-shadow--2dp" ]
            [ div [ class "mdl-card__title" ]
                [ h2 [ class "mdl-card__title-text" ] [ text "Checksum Parser" ]
                ]
            , div [ class "mdl-card__supporting-text" ] [ text "1. click 'Load Cached'", br [] [], text "2. click 'Parse Checksums'" ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "mdl-textfield__input"
                        , rows 3
                        , placeholder "Paste checksums text here"
                        , required True
                        , onInput InputChecksumsText
                        ]
                        [ text model.checksumText ]
                    ]
                , div [ class "mdl-card__supporting-text" ]
                    [ div [ class "textarea_label" ] [ text "Checksum Value:" ]
                    , text <| String.fromInt model.characterCount
                    , div [ class "textarea_label" ] [ text "Checksum Character Matches:" ]
                    , text <| model.matchedChecksumCharacters
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
                    , onClick ParseChecksumsText
                    ]
                    [ text "Parse Checksums" ]
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


checksumLength : Int
checksumLength =
    26


checksums : String
checksums =
    """naosmkcwtdbfivxuphzweraljq
nvssmicltdbfiyxuphzgeraljq
nvosmkcwwdbfiyxuphzeeraljx
nvosmkcqtdbfiyxupkzgeraljw
qvosmkcwtdbhiyxuphzgeraljh
nvocqkcktdbfiyxuphzgeraljq
nvosmhcwtdbfiyxmphzgekaljq
nvosmkcwtdbfuyxwpszgeraljq
nvosmocwtcbfiyxupfzgeraljq
nvosmkcwtdbfiyxubczgeraljv
nvosmkswtdbfiyxuphzgeruejq
nlosmkcwtqbfiyxuphzgyraljq
nvosmkcwtdbficxuphzgwraljk
nvosmkkwtdbfiyxxphzgeralcq
vvosmkcetdbfiyxumhzgeraljq
evosmkcdtdbfiyxuphkgeraljq
nvosmkvwtdbkiyxuphzgeraejq
nvoszkcwtdbfimxuphzgeraljb
nvozmkcwtdbfiyxuphzgrrcljq
nvosvacwtdbfiyxuphzgeralzq
nvosmkcwgdofiyxuthzgeraljq
nvosmkcwasbfiyxuphzgeradjq
nvosmkcatobfiyxtphzgeraljq
nvosmkewtdsfiyxuphzgekaljq
tvormkcwtdbfiyxuphzieraljq
nvosgkcwtdbfiyxuuhzgeraqjq
nvosmkcwtdbqiwxuphzgeralvq
nvosmkcwtybfiydcphzgeraljq
nvosnkcwtdbfiyxuphzulraljq
nvosmkcwtdbbiyuupnzgeraljq
nvosmwcwtdbfiyxuphzneraojq
nvohlkcwtdbftyxuphzgeraljq
nvasmkcwbdbfiyiuphzgeraljq
nvosmkujtdbfiyxuphzgeraljz
nvosmgcstdbfiyxuphzgeraljd
nvoswkcwtsbziyxuphzgeraljq
nvosmmcwtdbfiyxupbzzeraljq
nvosmkcwtdbfifxulhzgeralji
nvolmkcwtdbmiyxuphzgeraljv
lvnsmkcwtdbfiyxuphzzeraljq
nvqsmkcwtdbfiyxuphageralfq
nvosmkcwtdmfiyluphzgeralzq
nvommvcwtdbfiyxupjzgeraljq
naosmkcwtdbfsyxuphzgsraljq
avosmkcwtdbfiyxuphzgebafjq
ndozmkcwtdbfiyxuhhzgeraljq
nvosmkcwtubfiyxuphooeraljq
nvosmkcwtdbliyxuphzgmraljx
nvosmkcuddbfimxuphzgeraljq
wvosmkzwrdbfiyxuphzgeraljq
nvosmkcqtdbfiyxupjzgeraijq
nvosbkcwtdbfiyduphzgeruljq
yzosmkcntdbfiyxuphzgeraljq
nvolmkcwtdbfiyxuphugeralfq
nvrsmkcwtdbjiyxuphzgejaljq
nvgsmkcwtdbfiyxuphoglraljq
nvosmkcwtdbfioxuphzgezalhq
nvosjkcwtdbfipxuphzgekaljq
nvosmkcwtabfiyxlpazgeraljq
nvosmkfwtpnfiyxuphzgeraljq
nvokmbcwtdbeiyxuphzgeraljq
nvosmkcwtdbfiyxupmzgmlaljq
nvosmkcwtdhfiykurhzgeraljq
nvosmkcwwdbfiyxumhzgiraljq
cvosmscwtdbfikxuphzgeraljq
nvosmkcwtdnzirxuphzgeraljq
nvosmscwtdbfiyxuuhbgeraljq
nvosmkcwtdbfidxpphzgeraajq
nvosmkcwtdbfiyxuqhzgurcljq
nvosmkcwtekfiyxrphzgeraljq
ntosmkcwtpqfiyxuphzgeraljq
nvosmkcdtdbfhyxsphzgrraljq
nvolmkkwtdbfiyxuphzgeralgq
nvosmrcwtdbfiyxuphzgefkljq
nvoxmkcwtdbfiysuphzeeraljq
nvjsmkswtdbfiyxuphzqeraljq
nvosmkcetdbfiyfuphdgeraljq
nvosmkkwtpbfsyxuphzgeraljq
nvosdgcwtdbfiyxupyzgeraljq
nvosmkcwudbfiyzvphzgeraljq
nvosmkcwtlbfiyxupkzgerzljq
nvosmkcwtdbfiywuphyzeraljq
nvocmkcwtdufiyxukhzgeraljq
nvosmkcwtdqfiyxuphzgevaxjq
nvosvkcwtdbgiyxuphzgeralzq
nqosmkcwtdbfiyxuphzeeraljr
nvosmkcetdbfiyxuphzgeroljo
nvosmkcwtdvfiyxuphzceraliq
nvosmkcwtnxfiyxuphzgyraljq
nvosmkfwtdefiyxupxzgeraljq
nvosmacwtdbfiyxuphzseragjq
nvpsmkcwtdbfzyxuvhzgeraljq
nvormkcwtdbfiyxuphzairaljq
rvysmkcwtdbfmyxuphzgeraljq
nvosmscwzdbfiyxuphzgerbljq
nvosmkcwtdufmyxuphzqeraljq
nvosmkcwtxbfiyxxphzgeralxq
nvosmkcwtdbsiyxupsfgeraljq
nvosmccwtdbfiqxuthzgeraljq
nvosmtcwtqbuiyxuphzgeraljq
nvosmkcwtdbfiysurbzgeraljq
nvowmkcwtdbfiyxuywzgeraljq
xvosmkcktdbfiyxuhhzgeraljq
nvosmkgwsdbfiyxmphzgeraljq
jvofmkcwtdbfiyxupyzgeraljq
nvozakcwtdbfiexuphzgeraljq
nvosmkcptdbfiyxuphzgepaljn
nvosmkcwtdbpiyxuphzgeraxjw
nvoszkcwtdbfiyjuphzeeraljq
nvosmkcwtdbfiyxuppzoeraejq
nvosmkiytdbfiyhuphzgeraljq
nvosmkcwtdvfiywupyzgeraljq
nvosmecwtdofiyxuphzgeralja
nvosmkqwtdbfixxuphzgeraojq
nvosmkwwtdbfiyxfpdzgeraljq
nvosmkgwtdbfiyzupwzgeraljq
nmosmucwtdvfiyxuphzgeraljq
nvosmdcwtdbmiyxuphzveraljq
wvosmkcwtpbfiyxuphzgetaljq
nvosmmcwtdlfbyxuphzgeraljq
nvosmkcwtabmiexuphzgeraljq
nvosqpcwtdbfiyxuphzgqraljq
nvosmecwjdbfiyxuphzgeraljk
nyohmkcwtdbfiyxuphzgzraljq
nlosmkcwtkbfiyxuphzgeraejq
nvosmkcwrdbliyxuphzgerpljq
nvusmkzwtdbfxyxuphzgeraljq
nvosmkcwtdbfiyxuhizgerazjq
nvosmkhptdbfbyxuphzgeraljq
nvosmfcwtdbgiyxupdzgeraljq
nvosmkmwtdbfiyxuphzgevalpq
nvosmkcwtdwfiyxuphzherjljq
nvosmkcwjwbfiyxuphzgeualjq
nvosmkcwxdbflymuphzgeraljq
nvosmkcwpdbriyxuphzoeraljq
nvoshkcwcdbfiyxuphzgeravjq
nvosmkcetcbfiyxgphzgeraljq
nvosmkcwtdyfiyxuphzgerwqjq
nuosmkcwedbfiyxurhzgeraljq
nvosmkcwtdbfiixuphzctraljq
nvoszkcwtdbfwyxuphzgerpljq
nvormkcwtdbfiyxuphzgeralzn
nvosmkyttdbfiywuphzgeraljq
nvosmkcwtdbhiyxupazgeralhq
nvotmkcwtdbfiyxuphzgevalbq
nvosmkcwedbfiyxuphzguraljr
nvssmkcwtdbfiyxushzgeralbq
nvosmkcwtdeziyxuphzgeralhq
nvogmkcwtdbfiyxuphzgerrxjq
ncormkcwtdbfiyxuphzgeraloq
nvosmkcwbdbfiyeuphzgerqljq
nvosxkcwtdbfsyxupfzgeraljq
nvohmkcwtdbfiyxuphzseraajq
nvoscdcwtdbfiyxuphzgeralqq
neosmkcwtdbfiyxuchzgeralgq
njosmvcwpdbfiyxuphzgeraljq
nvosmkcwtwbfiyxuphzgehamjq
nvosmkcwtdbfiyxushzgejaljv
nvosmkcwodbfiyxuphzgeryqjq
nvoymqcwtdbfiyxuphzgeralbq
nvosmkcwtdjfiyxuphzgesaljb
nvjsmdcwedbfiyxuphzgeraljq
nvosmkcwydbfiyxuihzmeraljq
nvrsmkcwtdifiyxuphzgqraljq
nposmkcwtdbfiyxiohzgeraljq
dvosmkcwtdbfiyxuphzrvraljq
pvosmkcwudbfsyxuphzgeraljq
noosmkcwtdbfiyxuphtgexaljq
nvosmkcwtdbfiaxuphyferaljq
nvhsmlcwtdbfiyxuphzgeualjq
nvosekcwtdbbiyxuphzgerabjq
nvosvkcitdbfiyxuphzgerarjq
nvotmkkwtdbfiyxuphzgeraljj
nvosmecwtdbfiyxuphzgyralwq
hvosmkcwtdbfiyxuphzysraljq
nvosmkcvtdbfiyxlphzgeraljb
nvosmkcwttbfiyxuphngtraljq
nvoslkcwtdbfiyxuphzqeraljr
nxosmkcwtdbfibxuphzgrraljq
nvokmkhwtdbfiyxuphzgwraljq
nvosmkfwtdbfiyxuphzgdraljo
nvcsmkcwtdbfibxuphzgeraljl
nvosmkcwtdcfiaxuphzeeraljq
wvosmkcwtdbyiyxjphzgeraljq
nyosmbcwtjbfiyxuphzgeraljq
nvosmkcwtdbiiyxuahzieraljq
nqosmkcwtdbfiyxuyhzgerapjq
nvosmkcwtdbfiyxuwhzzetaljq
nvosmkcwfgbfiyxuphzgerrljq
nvosmbcwtdbfipxuphzderaljq
nvosmkcwtdgfiyxupdzgerjljq
noosmkcwtdcfiyxuphlgeraljq
nvonmkcutdbfiyxuphzieraljq
nvocmkcwtdbfiyyuphageraljq
nvosmkcwtdbfoyxuphzneraqjq
nvoskkcwtdbtiyxuphzgevaljq
ocosmkswtdbfiyxuphzgeraljq
nvosmkcqtdbfiyxfvhzgeraljq
noosmkcwtdbfiyquphzberaljq
nvosmkcwttbfijxuchzgeraljq
nvogmkcwtdbfiyxupazgeralaq
nvqsmkcwtdbfikxuphzgeraliq
nvosmkuwtdbfiyxuphzjwraljq
nyosmhcwtdbfiyxuphzgereljq
nvosmncwtdbfietuphzgeraljq
gvpsmkcwtdbfiyxuyhzgeraljq
nvozmkewtlbfiyxuphzgeraljq
nvostkcltpbfiyxuphzgeraljq
nvosmkcwtdbdiyxuphzgehaljz
nvosmkcwtjbziyxuphzgexaljq
nvosmkcwtdbfiyptphzggraljq
nvosmkcwtdbliyxupjzgebaljq
nvosmkawtdbfiyxupxzgtraljq
vvosmkcwtdbfiyxfphzperaljq
nvosmkawtdbfiyxutczgeraljq
nvosmkcbtdbuiyxrphzgeraljq
nvbsmkcwtdbfiyxdphzgerasjq
nvosnkcwqdsfiyxuphzgeraljq
nvosmkcwtdbfiyxwphzgzzaljq
nvosmkcwtdbffyquphzgeralcq
nvosmkcwtzbfiyxdphzgzraljq
nvysmkcwtdbfiycvphzgeraljq
nvowmkcwtdbfiycuyhzgeraljq
nvosbkcwtdbfiyiuphzgeraqjq
nvosmecwtdbfiyxupqzmeraljq
nvosmkcdtdbfhyxsphzgeraljq
nmosmkcwtdbziyxuphzgercljq
nvosmkcwtdbfiyxupfmgersljq
nvosmkcvtdbpyyxuphzgeraljq
nvosmkcwtkbfiyaupxzgeraljq
nvosmkcwtzbiiyxuphzgerazjq
nvoxmkcwtdbfiyxuphztegaljq
nvonmkcwtdafiyxuphzgerkljq
rvommkcwtdbfiyxzphzgeraljq
nvosmkcwthbfiysuphzgeraxjq
nvosmkcwtdbfnyxuphzgerccjq
nrosmzcwtdbfiyxuphkgeraljq
nvolmkcdtdbfiyxuphtgeraljq
nvosfkcwtdbfiyeuphcgeraljq
nvowmkcwtdbfhyxuphzgerafjq
gvosmkcwtdbfiyxupbpgeraljq
nvosmkcwtdbkiyxuphegebaljq
nvommufwtdbfiyxuphzgeraljq
uvksmkcwtdbfiysuphzgeraljq
nvosmkcwevbfiyxuphtgeraljq
nvosmkcmtdbfiycuphzgeraxjq
nvcsxkcwtdbfiyxuphzgeraljn
nvosmkcwtdbtiymuphzgeraltq
nvosmfcwtdlfjyxuphzgeraljq
svosmkcitdbfiyxuphzgsraljq"""
