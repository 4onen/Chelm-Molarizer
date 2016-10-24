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
    case extractQuant word of
        Result.Ok qnt ->
            Result.Ok (Grams qnt)
        Result.Err error ->
            Result.Err error


decodeLiters : String -> Result String Quantity
decodeLiters word = 
    case extractQuant word of
        Result.Ok qnt ->
            Result.Ok (Liters qnt)
        Result.Err error ->
            Result.Err error

decodeMolals : String -> Result String Quantity
decodeMolals word =
    case extractQuant word of
        Result.Ok qnt ->
            Result.Ok (Molarity qnt)
        Result.Err error ->
            Result.Err error

extractQuant : String -> Result String Float
extractQuant word =
    let
        numberMatch = List.head (Regex.find (Regex.AtMost 1) (Regex.regex "(\\d+\\.?\\d*)([A-z]*)([Mgm])") word)
        numberString = 
            case numberMatch of
                Maybe.Just match ->
                    List.head match.submatches
                Maybe.Nothing ->
                    Maybe.Nothing
        siMult =
            case numberMatch of
                Maybe.Just match ->
                    case List.head (Maybe.withDefault [] (List.tail match.submatches))  of
                        Maybe.Just si ->
                            SI.multiplierFromSI (Result.withDefault SI.UnitUnit (SI.extractSIUnit (Maybe.withDefault "" si)))
                        Maybe.Nothing ->
                            1
                Maybe.Nothing ->
                    1
    in
        case numberString of
            Maybe.Just match ->
                case (String.toFloat (Maybe.withDefault "" match)) of
                    Result.Ok number ->
                        Result.Ok (number*siMult)
                    Result.Err error ->
                        Result.Err error
            Maybe.Nothing ->
                Result.Err "No number match found!"



decodeMolecule : String -> Result String Quantity
decodeMolecule word =
    Result.Err "[A-Z][a-z]?\\d*|\\((?:[^()]*(?:\\(.*\\))?[^()]*)+\\)\\d+"
