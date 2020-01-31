module Attribute exposing (..)

import IO exposing (IO)
type alias Attribute parent val =
    {
        name: String,
        getter: parent -> val,
        def : IO val val
    }

type AttributeArgument a b =
    Name String
    | Getter (a->b)
    | Def (IO b b)

name : String -> AttributeArgument a b
name n =
    Name n

getter : (a -> b) -> AttributeArgument a b
getter g =
    Getter g

def : IO b b -> AttributeArgument a b
def io =
    Def io

fromList : List (AttributeArgument a b) -> Attribute a b
fromList values =
    default

default : (a->b) -> Attribute a b
default get = 
    {
        getter = get,
        name = "",
        def = entity
    }