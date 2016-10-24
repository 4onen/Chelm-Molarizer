module MoleculeParser exposing (parseMolecule)

import Result
import String
import Char

type alias Atom = 
    { symbol : (Result String String)
    , count : Int
    , characters : Int
    }

parseMolecule : String -> Result String (List String)
parseMolecule input =
    if String.isEmpty input then
        Result.Ok []
    else if (String.left 1 input) == "(" then
        Result.Err "Unimplemented \"(\""
    else if (String.left 1 input) == ")" then
        Result.Err "Unimplemented \")\""
    else if String.all Char.isUpper (String.left 1 input) then
        let
            atom = subParseAtom input
        in
            case atom.symbol of
                Result.Ok symbol ->
                    let
                        myResult = (List.repeat atom.count symbol)
                        theirResult = (parseMolecule (String.dropLeft atom.characters input))
                    in
                        case theirResult of
                            Result.Ok theirList ->
                                Result.Ok (List.append myResult theirList)
                            Result.Err error ->
                                theirResult
                Result.Err error ->
                    Result.Err error
    else
        Result.Err ("Unparseable symbol encountered: "++(String.left 1 input))

subParseAtom : String -> Atom
subParseAtom input = 
    if String.isEmpty input then
        Atom (Result.Err "No atom present.") 0 0
    else if not (String.all Char.isUpper (String.left 1 input)) then
        Atom (Result.Err "Imparseable character.") 0 0
    else
        let
            symbolLength = 
                if String.all Char.isLower (String.right 1 (String.left 2 input)) then
                    2
                else
                    1 
            symbol = String.right symbolLength input
        in
            Atom (Result.Ok symbol) 0 symbolLength

                