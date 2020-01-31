module Entity exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline
import Json.Encode exposing (null)
import Json.Encode.Extra


type alias View flag mediator target =
    { translate : flag -> mediator -> target
    , entity : Entity target
    }



{- int_view : View flag mediator Int
int_view =
    {
        entity= Type {

        },
        translate = {}
    } -}






type alias RecordEncoder a =
    List (a -> ( String, Json.Encode.Value ))


type Entity flag
    = Record (RecordBuilder flag flag)
    | Type (TypeBuilder flag flag)


type alias RecordBuilder decoded built =
    { name : String
    , decoder : Decoder decoded
    , encoder : RecordEncoder built
    , attributes : List String
    , toString : Dict String (built -> String)
    }


type alias TypeBuilder a b =
    { name : String
    , decoder : Decoder b
    , encoder : Encoder a
    , toString : a -> String
    }


type alias Parser a =
    a -> String



-- maybe : Parser a -> Parser (Maybe a)
-- maybe parse target =
--     Maybe.map parse target
--     |> Maybe.withDefault ""
-- connect : String -> Entity a b mediator -> Entity c d mediator ->


maybe : Entity flag -> Entity (Maybe flag)
maybe p =
    case p of
        Type b ->
            Type
                { name = "maybe " ++ b.name
                , decoder = nullable b.decoder
                , encoder = Json.Encode.Extra.maybe b.encoder
                , toString =
                    \x ->
                        case x of
                            Just v ->
                                b.toString v

                            Nothing ->
                                ""
                }

        Record e ->
            Record
                { name = "maybe " ++ e.name
                , decoder = nullable e.decoder
                , encoder =
                    [ \entity ->
                        ( e.name
                        , case entity of
                            Just v ->
                                Json.Encode.object (List.map (\x -> x v) e.encoder)

                            Nothing ->
                                null
                        )
                    ]
                , attributes = e.attributes
                , toString = Dict.map (\k v a -> Maybe.map v a |> Maybe.withDefault "") e.toString
                }


int : Entity Int
int =
    Type
        { name = "int"
        , decoder = Json.Decode.int
        , encoder = Json.Encode.int
        , toString = String.fromInt
        }


string : Entity String
string =
    Type
        { name = "string"
        , decoder = Json.Decode.string
        , encoder = Json.Encode.string
        , toString = \x -> x
        }



-- attribute :
--     String
--     -> (built -> newtype)
--     -> Entity newtype newmodel mediator
--     -> RecordBuilder (newtype -> newdecoded) built (newtype -> newmodel) mediator
--     -> RecordBuilder newdecoded built newmodel mediator
-- attribute name getter newentity old =
--     case newentity of
--         Type t ->
--             subtype name getter t old
--         Record r ->
--             subrecord name getter r old
-- attribute :
--     String
--     -> (built -> newtype)
--     -> Entity newtype newmodel mediator
--     -> RecordBuilder (newtype -> newdecoded) built (newtype -> newmodel) mediator
--     -> RecordBuilder newdecoded built newmodel mediator
-- attribute name flag_getter e old =
--     case e of
--         Type b ->
--             { name = old.name
--             , attributes = name :: old.attributes
--             , toString = Dict.insert name (\x -> b.toString (flag_getter x)) old.toString
--             , decoder =
--                 old.decoder
--                     |> Json.Decode.Pipeline.required name b.decoder
--             , encoder = match_encoder flag_getter name b.encoder old.encoder
--             , model = match_model (\x y -> Just (flag_getter x)) old.model
--             }
--         Record r ->
--             {
--                 name = old.name
--                 , attributes = name :: old.attributes
--                 -- , toString = Dict.insert name (\x -> r.toString (flag_getter x)) old.toString
--                 , toString = adapt_toString r.toString old.toString
--                 , decoder =
--                 old.decoder
--                     |> Json.Decode.Pipeline.required name r.decoder
--                 , encoder = match_encoder flag_getter name (adapt_encoder r.encoder) old.encoder
--                 , model = match_model (\x y -> Just (flag_getter x)) old.model
--             }
-- consume :
--     String
--     -> Consumer delta target
--     -> RecordBuilder (delta -> target) built
--     -> RecordBuilder target built
-- consume name con old =
--     { name = old.name,
--         attributes = name :: old.attributes,
--         toString = old.toString,
--         decoder = old.decoder
--                   |> con.decoder,
--         encoder = old.encoder
--     }
-- -- Replace userId by user
-- relationshipConsuming :
--     String -- name
--     -> (built -> key) -- getter of the subrecord
--     -> (key -> mediator -> Maybe value) -- getter of the other value(s)
--     -> Entity valueflag value mediator -- other description
--     -> RecordBuilder (key -> newdecoded) built (value -> newmodel) mediator
--     -> RecordBuilder newdecoded built newmodel mediator
-- relationshipConsuming name rgetter mgetter sub old =
--     { name = old.name
--     , attributes = name :: old.attributes
--     , toString = Dict.insert name (\x -> sub.name) old.toString
--     , decoder = old.decoder |> Json.Decode.Pipeline.required name sub.decoder
--     , encoder = match_encoder rgetter name (adapt_encoder sub.encoder) old.encoder
--     , model = \flag mediator -> Maybe.map2 (\x y -> y x) (mgetter (rgetter flag) mediator) sub.model
--     }
-- Add calculated field via primary key
-- relationshipNonConsuming :
--     String -- name
--     -> (built -> key) -- getter of the subrecord
--     -> (key -> mediator -> Maybe value) -- getter of the other value(s)
--     -> Entity valueflag value mediator -- other description
--     -> RecordBuilder decoded built (value -> newmodel) mediator
--     -> RecordBuilder decoded built newmodel mediator
-- relationshipNonConsuming name rgetter mgetter other old =
--     {
--         name = old.name,
--         attributes = name :: old.attributes,
--         toString = Dict.insert name (\x -> name) old.toString,
--         decoder = old.decoder,
--         encoder = old.encoder,
--         model = \built mediator -> Maybe.map2 (\argument oldf -> oldf argument) (mgetter (rgetter built) mediator) (old.model built mediator)
--     }


new : a -> RecordBuilder a b
new entity =
    { name = ""
    , decoder = succeed entity
    , encoder = []
    , attributes = []
    , toString = Dict.empty
    }


finish : RecordBuilder a a -> Entity a
finish e =
    Record e


encode : RecordBuilder a b -> b -> Json.Encode.Value
encode entity instance =
    Json.Encode.object
        (List.map (\x -> x instance) entity.encoder)



-- HELPER


match_encoder : (a -> b) -> String -> (b -> Json.Encode.Value) -> List (a -> ( String, Json.Encode.Value )) -> List (a -> ( String, Json.Encode.Value ))
match_encoder getter name encoder old =
    (\x -> ( name, encoder (getter x) )) :: old


adapt_encoder : RecordEncoder a -> Encoder a
adapt_encoder r =
    \encodable -> Json.Encode.object (List.map (\y -> y encodable) r)


adapt_toString : Dict String (a -> String) -> Dict String (b -> String) -> Dict String (b -> String)
adapt_toString oldToString newToString =
    newToString


match_model : (flag -> mediator -> Maybe a) -> (flag -> mediator -> Maybe (a -> model)) -> (flag -> mediator -> Maybe model)
match_model getter old_model =
    \flag mediator ->
        let
            value =
                getter flag mediator

            func =
                old_model flag mediator
        in
        Maybe.map2 (\x y -> x y) func value



-- type alias Encoder a =
--     a -> Json.Encode.Value
-- type alias Entity a b =
--     {
--         name : String,
--         decoder : Decoder b,
--         encoder : List ((a -> (String, Json.Encode.Value))),
--         attributes : List String,
--         toString : Dict String (a -> String)
--     }
-- type alias Parser a =
--     a -> String
-- maybe : Parser a -> Parser (Maybe a)
-- maybe parse target =
--     Maybe.map parse target
--     |> Maybe.withDefault ""
-- int : Parser Int
-- int =
--     String.fromInt
-- string : Parser String
-- string x =
--     x
-- attribute : String -> (a -> b) -> Parser b -> Entity a c -> Entity a c
-- attribute name getter parser old =
--     {old | attributes = name :: old.attributes
--          , toString = Dict.insert name (\x -> parser (getter x)) old.toString}
-- new : c ->  Entity a c
-- new entity =
--     {
--         name = "",
--         decoder = succeed entity,
--         encoder = [],
--         attributes = [],
--         toString = Dict.empty
--     }
-- encode : Entity a c -> a -> Json.Encode.Value
-- encode entity instance =
--     Json.Encode.object
--         (List.map (\x -> x instance) entity.encoder)
