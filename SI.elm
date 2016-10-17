module SI exposing (..)

import Result

type SI
    = Exa
    | Peta
    | Tera
    | Giga
    | Mega
    | Kilo
    | Hecto
    | Deca
    | UnitUnit
    | Deci
    | Centi
    | Milli
    | Micro
    | Nano
    | Pico
    | Femto
    | Atto

extractSIUnit : String -> Result String SI
extractSIUnit unit =
    if unit == "E" then Result.Ok Exa
    else if unit == "P" then Result.Ok Peta
    else if unit == "T" then Result.Ok Tera
    else if unit == "G" then Result.Ok Giga
    else if unit == "M" then Result.Ok Mega
    else if unit == "k" then Result.Ok Kilo
    else if unit == "h" then Result.Ok Hecto
    else if unit == "da" then Result.Ok Deca
    else if unit == "" then Result.Ok UnitUnit
    else if unit == "d" then Result.Ok Deci
    else if unit == "c" then Result.Ok Centi
    else if unit == "m" then Result.Ok Milli
    else if unit == "μ" then Result.Ok Micro
    else if unit == "n" then Result.Ok Nano
    else if unit == "p" then Result.Ok Pico
    else if unit == "f" then Result.Ok Femto
    else if unit == "a" then Result.Ok Atto
    else Result.Err "Invalid unit"

multiplierFromSI : SI -> Float
multiplierFromSI unit =
    case unit of
        Exa -> 10^18
        Peta -> 10^15
        Tera -> 10^12
        Giga -> 10^9
        Mega -> 10^6
        Kilo -> 10^3
        Hecto -> 10^2
        Deca -> 10
        UnitUnit -> 1
        Deci -> 10^(-1)
        Centi -> 10^(-2)
        Milli -> 10^(-3)
        Micro -> 10^(-6)
        Nano -> 10^(-9)
        Pico -> 10^(-12)
        Femto -> 10^(-15)
        Atto -> 10^(-18)

multiplierToSI : SI -> Float
multiplierToSI unit =
    case unit of
        Exa -> 10^(-18)
        Peta -> 10^(-15)
        Tera -> 10^(-12)
        Giga -> 10^(-9)
        Mega -> 10^(-6)
        Kilo -> 10^(-3)
        Hecto -> 10^(-2)
        Deca -> 10^(-1)
        Deci -> 10
        UnitUnit -> 1
        Centi -> 10^2
        Milli -> 10^3
        Micro -> 10^6
        Nano -> 10^9
        Pico -> 10^12
        Femto -> 10^15
        Atto -> 10^18