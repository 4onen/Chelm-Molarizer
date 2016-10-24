import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events
import String
import Regex

import PeriodicTable
import Chelm
import SI



main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }




-- MODEL
type alias Model = 
    { inputNum : String
    , inputUnits : String
    , inputSecondNum : String
    , inputSecondUnits : String
    , results : List (String, String)
    }

model : Model
model = 
    { inputNum = ""
    , inputUnits = ""
    , inputSecondNum = ""
    , inputSecondUnits = ""
    , results = []
    }




-- UPDATE
type Msg
    = InputNumChanged String
    | InputUnitsChanged String
    | InputSecondNumChanged String
    | InputSecondUnitsChanged String
    | RunCommand

update : Msg -> Model -> Model
update msg model =
    case msg of
        InputNumChanged contents ->
            { model | inputNum = contents }
        InputUnitsChanged contents ->
            { model | inputUnits = contents }
        InputSecondNumChanged contents ->
            { model | inputSecondNum = contents }
        InputSecondUnitsChanged contents ->
            { model | inputSecondUnits = contents }
        RunCommand ->
            { model | inputNum = ""
            , inputUnits = ""
            , inputSecondNum = ""
            , inputSecondUnits = ""
            , results = [(computeCommand model)] ++ model.results
            }

computeCommand : Model -> (String, String)
computeCommand model =
    if "TABLE" == (String.toUpper model.inputNum) then
        (model.inputNum++model.inputUnits, toString PeriodicTable.periodicTable)
    else if Regex.contains (Regex.regex "\\d+.?\\d*") model.inputNum then
        if model.inputSecondNum /= "" then
            (model.inputNum ++ model.inputUnits ++ " of " ++ model.inputSecondNum ++ model.inputSecondUnits
            , toString(chemistry model.inputNum model.inputUnits model.inputSecondNum model.inputSecondUnits)
            )
        else
            ("Weird shape:"++model.inputNum ++ model.inputUnits ++ " of " ++ model.inputSecondNum ++ model.inputSecondUnits
            , toString(chemistry model.inputNum model.inputUnits model.inputSecondNum model.inputSecondUnits)
            )

    else
        (model.inputNum++model.inputUnits, "Most command parsing not yet implemented.")



-- VIEW
view : Model -> Html Msg
view model = 
    div []
        [ div [] 
            [ Html.input [ type' "text", placeholder "Number", value model.inputNum, Html.Events.onInput InputNumChanged ] []
            , Html.input [ type' "text", placeholder "Units", value model.inputUnits, Html.Events.onInput InputUnitsChanged ] []
            , div [] (viewSecondNumBox model.inputUnits)
            , button [ Html.Events.onClick RunCommand ] [ text "Molarize" ]
            ]
        , div [] (viewPastCommands model)
        ]

viewPastCommands : Model -> List (Html Msg)
viewPastCommands model =
    List.map (viewPastCommand) model.results

viewPastCommand : (String, String) -> Html Msg
viewPastCommand (input, output) =
    div [] 
        [ p [] [ text ("> " ++ input) ]
        , p [] [ text output ]
        ]

viewSecondNumBox : String -> List (Html Msg)
viewSecondNumBox units = 
    let
        unit = String.right 1 units
    in
        if unit == "L" then
            displaySecondNumBox "M"
        else if unit == "M" then
            displaySecondNumBox "L"
        else if unit == "g" then
            displayMoleculeBox
        else
            []
            
displaySecondNumBox : String -> List (Html Msg)
displaySecondNumBox requiredUnit = 
    [ Html.text " of "
    , Html.input [ type' "text", placeholder "Value 2", Html.Events.onInput InputSecondNumChanged ] []
    , Html.input [ type' "text", placeholder "Units 2 (Just the SI part)", Html.Events.onInput InputSecondUnitsChanged ] []
    , Html.text requiredUnit
    ]

displayMoleculeBox : List (Html Msg)
displayMoleculeBox =
    [ Html.text " of "
    , Html.input [ type' "text", placeholder "Molecule", Html.Events.onInput InputSecondUnitsChanged ] []
    ]

-- CHEMISTRY
chemistry : String -> String -> String -> String -> Result String Chelm.Quantity
chemistry inputNum inputUnits inputSecondNum inputSecondUnits = 
    let
        num = String.toFloat inputNum
        unitMultiple = SI.multiplierFromSI (Result.withDefault SI.UnitUnit (SI.extractSIUnit (String.dropRight 1 inputUnits)))
        unitType = String.right 1 inputUnits
    in
        if (inputSecondNum == "" && inputSecondUnits == "") then
            case num of
                Result.Ok val ->
                    if unitType == "g" then
                        Result.Ok (Chelm.Grams (val*unitMultiple))
                    else if unitType == "L" then
                        Result.Ok (Chelm.Liters (val*unitMultiple))
                    else if unitType == "M" then
                        Result.Ok (Chelm.Molarity (val*unitMultiple))
                    else
                        Result.Err "No valid unit type. I support g, L, or M."
                Result.Err error ->
                    Result.Err ("An error occurred: "++error)
        else if inputSecondNum /= "" then
            case num of
                Result.Ok val ->
                    let
                        num2 = String.toFloat inputSecondNum
                        unitMultiple2 = SI.multiplierFromSI (Result.withDefault SI.UnitUnit (SI.extractSIUnit inputSecondUnits))
                    in
                        case num2 of
                            Result.Ok val2 ->
                                if unitType == "L" then
                                    Result.Ok (Chelm.Mols (chemistryLiterMolalToMols (val*unitMultiple) (val2*unitMultiple2)))
                                else if unitType == "M" then
                                    Result.Ok (Chelm.Mols (chemistryLiterMolalToMols (val2*unitMultiple2) (val*unitMultiple)))
                                else
                                    Result.Err "Impossible unit reached interior chemistry parser"
                            Result.Err error ->
                                Result.Err error
                Result.Err error ->
                    Result.Err error
        else
            Result.Err "Molecular masses unimplemented."

chemistryLiterMolalToMols : Float -> Float -> Float
chemistryLiterMolalToMols liters molals =
    liters * molals
        
