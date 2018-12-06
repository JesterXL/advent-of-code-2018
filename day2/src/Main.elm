module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (foldl, length, partition, map, filter)
import String exposing (split, toList)
import Tuple exposing (first)


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


-- [['n'],['v','v'],['o'],['s'],['v','v'],['k'],['c'],['i','i'],['t'],['d'],['b'],['f'],['i','i'],['y'],['x'],['u'],['p'],['h'],['z'],['g'],['e'],['r','r'],['a'],['r','r'],['j'],['q']]
checkPartitionLol charList =
    map (\char -> partition (\letter -> letter == char) charList) charList
    |> map (\item -> first item)
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
            checkPartitionLol charList
        msg1 = log "demLetters" demLetters
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

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            let
                -- Attempt #1: The checksum 85272 is too damn high
                checksum = 
                    split "\n" checksums
                    |> map findDuplicateCharacters
                    |> foldl reduceCharacterMatches (CharacterMatch 0 0)
                    |> calculateChecksum
                um = log "checksum" checksum
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
