import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events
import String
import Regex

import PeriodicTable
import Chelm



main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }




-- MODEL
type alias Model = 
    { input : String 
    , results : List (String, String)
    }

model : Model
model = { input = "", results = []}




-- UPDATE
type Msg
    = InputChanged String
    | RunCommand

update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged contents ->
            { model | input = contents }
        RunCommand ->
            { model | input = "", results = [(computeCommand model)] ++ model.results}

computeCommand : Model -> (String, String)
computeCommand model =
    if "TABLE" == (String.toUpper model.input) then
        (model.input, toString PeriodicTable.periodicTable)
    else if Regex.contains (Regex.regex "\\d+.\\d*") model.input then
        (model.input, {-parseInputChemistry model.input)-} "")
    else
        (model.input, "Most command parsing not yet implemented.")



-- VIEW
view : Model -> Html Msg
view model = 
    div []
        [ div [] 
            [ Html.input [ type' "text", placeholder "Statement", value model.input, Html.Events.onInput InputChanged ] []
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