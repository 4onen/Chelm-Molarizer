module MoleculeParser exposing (..)


import String
import Char
import Result
import Regex


moleculeRegexString : String
moleculeRegexString = "[A-Z][a-z]?\\d*|\\([^()]*(?:\\(.*\\))?[^()]*\\)\\d+"
moleculeRegex : Regex.Regex
moleculeRegex = Regex.regex moleculeRegexString


parseMolecule : String -> Result String (List String)
parseMolecule input = 
    let
        elementList = List.map .match (Regex.find (Regex.All) moleculeRegex input)
        resultList = List.map subParseMolecule elementList
        failureList = List.filter isError resultList
        successList = List.map (Result.withDefault [""]) resultList
    in
        if not (List.isEmpty failureList) then
            Result.Err "Unspecified error occurred somewhere down the line -- Finish implementing error handling!"
        else
            Result.Ok (List.concat successList)


subParseMolecule : String -> Result String (List String)
subParseMolecule element =
    {-if (String.left 1 element) == "(" then
        let
            sansLeftParan = String.dropLeft 1 element
            splitList = String.split ")" sansLeftParan
            subMoleculeString = Result.fromMaybe "Bad submolecule string" (List.head splitList)
            subMolecule = Result.map parseMolecule subMoleculeString
            count = String.toInt (Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.tail splitList))))
        in
            Result.map List.concat (Result.map2 List.repeat count subMolecule)
    else-}
        subParseElement element

subParseElement : String  -> Result String (List String)
subParseElement element =
    if not (String.all Char.isUpper (String.left 1 element)) then
        Result.Err ("No atomic symbol found at input: "++element)
    else if String.any Char.isLower (String.left 2 element) then
        let
            count = Result.withDefault 1 (String.toInt (String.dropLeft 2 element))
            symbol = String.left 2 element
        in
            Result.Ok (List.repeat count symbol)
    else
        let 
            count = Result.withDefault 1 (String.toInt (String.dropLeft 1 element))
            symbol = String.left 1 element
        in
            Result.Ok (List.repeat count symbol)

isError : Result a b -> Bool
isError input =
    case input of
        Result.Ok data -> False
        Result.Err error -> True

isOk : Result a b -> Bool
isOk input =
    case input of
        Result.Ok data -> True
        Result.Err error -> False
