module Example exposing (..)

import IO exposing (..)

type alias Test =
    { ms : Maybe String
    , mi : Maybe Int
    , li : List Int
    }

s : IO String
s = string

i : IO Int
i = int

ms : IO (Maybe String)
ms = maybe string

mi : IO (Maybe Int)
mi = maybe i

test : IO Test
test = 
    entity Test
    |> attribute "ms" (maybe string) .ms
    |> attribute "mi" (maybe int) .mi
    |> attribute "li" (list int) .li
