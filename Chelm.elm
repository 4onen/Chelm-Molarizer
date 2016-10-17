module Chelm exposing (..)

import Regex
import String
import Result

import PeriodicTable
import SI

-- CHEMISTRY
type alias Symbol = String 
type Quantity
    = Grams Float
    | Liters Float
    | Molarity Float
    | Molecule (List Atom)

numberRegString : String
numberRegString = "\\d+\\.?\\d*"

numberRegex : Regex.Regex
numberRegex = Regex.regex numberRegString
gramRegex : Regex.Regex
gramRegex = Regex.regex (numberRegString++"[Mkmnpf]?g")
literRegex : Regex.Regex
literRegex = Regex.regex (numberRegString++"[Mkmnpf]?L")
molarityRegex : Regex.Regex
molarityRegex = Regex.regex (numberRegString++"[Mkmnpf]?M")

molarizeInputChemistry : String -> String
molarizeInputChemistry input =
    let 
        equation = List.map decodeQuantity (String.words input)
    in
        toString equation -- Not final behavior

decodeQuantity : String -> Result String Quantity
decodeQuantity word =
    if (Regex.contains gramRegex word) then
        decodeGrams word
    else if (Regex.contains literRegex word) then
        decodeLiters word
    else if (Regex.contains molarityRegex word) then
        decodeMolals word
    else
        Result.Err "Invalid Quantity -- No SI unit found."

decodeGrams : String -> Result String Quantity
decodeGrams word = 
    let
        givenVal = extractNumber word
        multipleSI = 1 -- Implement SI units
    in
        Grams (givenVal*multipleSI)


decodeLiters : String -> Result String Quantity
decodeLiters word = 
    Result.Err "unimplemented"

decodeMolals : String -> Result String Quantity
decodeMolals word =
    Result.Err "unimplemented"

extractNumber : String -> Result String Float
extractNumber word =
    let
        numberString = List.head (Regex.find (Regex.AtMost 1) numberRegex word)
    in
        case numberString of
            Maybe.Just match ->
                String.toFloat match.match
            Maybe.Nothing ->
                Result.Err "No number match found!"

extractSI : String -> Char

decodeMolecule : String -> Result String Quantity
    Result.Err "https://stackoverflow.com/questions/9957939/chemical-formula-parser-c"