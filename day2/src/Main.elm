module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (foldl, length, partition, map, filter, head, reverse)
import String exposing (split, toList)
import Tuple exposing (first, pair)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Set exposing (Set, insert, empty)
import Char exposing (toCode, fromCode)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


type alias CharacterMatch =
    { char2 : Int
    , char3 : Int
    }


equals2 : Int -> Bool
equals2 value =
    value == 2


equals3 : Int -> Bool
equals3 value =
    value == 3


has2LettersThatAreTheSame : List Char -> Char -> Bool
has2LettersThatAreTheSame charList letter =
    partition (\char -> char == letter) charList
        |> first
        |> length
        |> equals2


has3LettersThatAreTheSame : List Char -> Char -> Bool
has3LettersThatAreTheSame charList letter =
    partition (\char -> char == letter) charList
        |> first
        |> length
        |> equals3


reduceCharacterMatch : CharacterMatch -> Bool -> Bool -> CharacterMatch
reduceCharacterMatch acc has2LettersSame has3LettersSame =
    if has2LettersSame && has3LettersSame then
        { acc | char2 = acc.char2 + 1, char3 = acc.char3 + 1 }

    else if has2LettersSame && has3LettersSame == False then
        { acc | char2 = acc.char2 + 1 }

    else if has2LettersSame == False && has3LettersSame then
        { acc | char3 = acc.char3 + 1 }

    else
        acc

last : List a -> Maybe a
last list =
    head (reverse list)



getDefultCharacter : List Char -> Char
getDefultCharacter list =
    Maybe.withDefault '?' (List.head list)

-- [['n'],['v','v'],['o'],['s'],['v','v'],['k'],['c'],['i','i'],['t'],['d'],['b'],['f'],['i','i'],['y'],['x'],['u'],['p'],['h'],['z'],['g'],['e'],['r','r'],['a'],['r','r'],['j'],['q']]
-- [['v','v'],['v','v'],['i','i'],['i','i'],['r','r'],['r','r']]
partitionTwosAndThress charList =
    map (\char -> partition (\letter -> letter == char) charList) charList
    |> map (\item -> first item)
    |> filter (\list -> length list == 2 || length list == 3)
    -- |> partition (\list -> length list == 2)
    -- |> headCharList
    -- |> Maybe.withDefault []
    -- |> reduceDuplicateLettersToSet

partitionTwos list =
    partition (\chars -> length chars == 2) list     

partitionThrees list =
    partition (\chars -> length chars == 3) list
        
    -- |> map length
    -- |> filter (\item -> item == 2 || item == 3)
    -- |> foldl (\item acc ->
    --             if item == 2 then
    --                 { acc | char2 = acc.char2 + 1}
    --             else if item == 3 then
    --                 { acc | char3 = acc.char3 + 1}
    --             else
    --                 acc) (CharacterMatch 0 0)


reduceCharListToCharacterMatches charList item acc =
    let
        has2LettersSame =
            has2LettersThatAreTheSame charList item

        has3LettersSame =
            has3LettersThatAreTheSame charList item

        demLetters = 
            partitionTwosAndThress charList
        -- msg1 = log "demLetters" demLetters
        the2 = log "the2" (partitionTwos demLetters)
        -- the3 = log "the3" (partitionThrees demLetters)
        -- twos = partitionTwos demLetters
        -- twosMapped = List.map (\i -> List.head i) twos
        -- m3 = log "m3" twosMapped
        -- that = foldl (\item2 acc2 ->
        --     pair (withDefault '?' (head item2) ) (withDefault '?' (last item2) ) :: acc2) [] demLetters
        -- msg3 = log "that" that
        -- msg2 = log "charList" charList
    in
    reduceCharacterMatch acc has2LettersSame has3LettersSame


findDuplicateCharacters : String -> CharacterMatch
findDuplicateCharacters string =
    let
        charList =
            toList string

        matches =
            foldl (reduceCharListToCharacterMatches charList) (CharacterMatch 0 0) charList
    in
    matches

reduceCharacterMatches : CharacterMatch -> CharacterMatch -> CharacterMatch
reduceCharacterMatches item acc =
    { acc | char2 = acc.char2 + item.char2, char3 = acc.char3 + item.char3}

calculateChecksum : CharacterMatch -> Int
calculateChecksum characterMatch =
    characterMatch.char2 * characterMatch.char3

setInsertRight : Set Char -> Char -> Set Char
setInsertRight set value =
    Set.insert value set

insertWithDefault : List Char -> Set Char -> Set Char
insertWithDefault listWithLetters letterSet =
    List.head listWithLetters
    |> Maybe.withDefault '?'
    |> setInsertRight letterSet

reduceDuplicateLettersToSet : List (List Char) -> Set Char
reduceDuplicateLettersToSet list =
    List.foldl insertWithDefault Set.empty list

partitionSameLetters charList letter =
    partition (\char -> char == letter) charList
    |> Tuple.first

-- has2LettersThatAreTheSame
filterOutSameLetters charList =
    List.map (partitionSameLetters charList) charList
    |> List.filter (\matchList -> List.length matchList > 1)

sortCharsAlphabetical : List Char -> List Char
sortCharsAlphabetical list =
    List.map Char.toCode list
    |> List.sort
    |> List.map Char.fromCode

-- condenseSameCharMatchesToSingles : List Char -> List Char

getFirstLetterOrNoClue : List Char -> Char
getFirstLetterOrNoClue list =
    Maybe.withDefault '?' (List.head list)

incrementDictValue : Dict Char Int -> Char -> Dict Char Int
incrementDictValue dict key =
    let
        currentValue = Maybe.withDefault 0 (Dict.get key dict)
    in
        Dict.update key (\maybeVal -> Just (currentValue + 1)) dict

count2And3CharactersInChecksum : List Char -> CharacterMatch
count2And3CharactersInChecksum checksumCharList =
    filterOutSameLetters checksumCharList
    |> List.foldl (\matchList foundDict -> incrementDictValue foundDict (getFirstLetterOrNoClue matchList)) Dict.empty
    |> Dict.foldl (\charKey charCount charMatch ->
        if charCount == 2 then
            { charMatch | char2 = charMatch.char2 + 1}
        else if charCount == 3 then
            { charMatch | char3 = charMatch.char3 + 1}
        else
            charMatch) (CharacterMatch 0 0)

combineCharacterMatches : List CharacterMatch -> CharacterMatch
combineCharacterMatches characterMatches =
    List.foldl (\match acc -> { acc | char2 = acc.char2 + match.char2, char3 = match.char3 }) (CharacterMatch 0 0) characterMatches

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            let
                -- Attempt #1: The checksum 85272 is too damn high!
                -- Attempt #2: The checksum 646 is too low, like my self-esteem in this language.
                -- charTotal = 
                --     List.take 10 (split "\n" checksums)
                --     |> List.map toList
                --     |> List.map sortCharsAlphabetical
                --     |> List.head
                --     |> Maybe.withDefault []
                --     |> filterOutSameLetters
                --     -- |> List.foldl (\matchList foundSet -> Set.insert (getFirstLetterOrNoClue matchList) foundSet) Set.empty
                --     |> List.foldl (\matchList foundDict -> incrementDictValue foundDict (getFirstLetterOrNoClue matchList)) Dict.empty
                --     |> Dict.foldl (\charKey charCount charMatch ->
                --         if charCount == 2 then
                --             { charMatch | char2 = charMatch.char2 + 1}
                --         else if charCount == 3 then
                --             { charMatch | char3 = charMatch.char3 + 1}
                --         else
                --             charMatch) (CharacterMatch 0 0)
                charTotal =
                    -- List.take 10 (split "\n" checksums)
                    split "\n" checksums
                    |> List.map toList
                    |> List.map sortCharsAlphabetical
                    |> List.map count2And3CharactersInChecksum
                    |> combineCharacterMatches

                um = log "charTotal" charTotal
            in
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


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