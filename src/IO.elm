module IO exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, map, null, nullable, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import List.Extra


type Encoder a
    = SingleEncoder (a -> Json.Encode.Value)
    | ListEncoder (List (a -> ( String, Json.Encode.Value )))


type alias PartialIO delta full =
    { decoder : Decoder delta

    -- name -> object -> StringRepr
    , toString : String -> full -> Maybe String
    , encoder : Encoder full
    }


type alias IO kind =
    PartialIO kind kind


type Reference kind
    = Reference String


int : IO Int
int =
    { decoder = Json.Decode.int
    , toString = \_ i -> Just (String.fromInt i)
    , encoder = SingleEncoder Json.Encode.int
    }


string : IO String
string =
    { decoder = Json.Decode.string
    , toString = \_ x -> Just x
    , encoder = SingleEncoder Json.Encode.string
    }


maybe : IO b -> IO (Maybe b)
maybe old =
    { decoder = nullable old.decoder
    , toString = \name -> Maybe.andThen (old.toString name)
    , encoder =
        case old.encoder of
            ListEncoder e ->
                SingleEncoder
                    (\x ->
                        case x of
                            Just v ->
                                Json.Encode.object (List.map (\y -> y v) e)

                            Nothing ->
                                Json.Encode.null
                    )

            SingleEncoder e ->
                SingleEncoder
                    (\x ->
                        Maybe.map e x
                            |> Maybe.withDefault Json.Encode.null
                    )
    }


list : IO b -> IO (List b)
list old =
    { decoder = Json.Decode.list old.decoder
    , toString = map_list_toString old.toString
    , encoder =
        SingleEncoder (Json.Encode.list (collapseEncoder old.encoder))
    }


dict : IO comparable -> IO b -> IO (Dict comparable b)
dict keys values =
    let
        decodeToMaybe : Decoder comparable -> ( String, b ) -> Maybe ( comparable, b )
        decodeToMaybe d ( s, obj ) =
            Json.Decode.decodeString d s
                |> Result.toMaybe
                |> Maybe.map (\x -> ( x, obj ))
    in
    { decoder =
        Json.Decode.keyValuePairs values.decoder
            |> Json.Decode.map (List.filterMap (decodeToMaybe keys.decoder))
            |> Json.Decode.map Dict.fromList
    , toString = map_dict_toString (\s -> Result.toMaybe <| Json.Decode.decodeString keys.decoder s) values.toString
    , encoder = map_dict_encoder keys values
    }


map_dict_encoder : IO comparable -> IO values -> Encoder (Dict comparable values)
map_dict_encoder keys values =
    SingleEncoder
        (\x ->
            Dict.toList x
                |> List.map (\( a, b ) -> ( keys.toString "" a, collapseEncoder values.encoder b ))
                |> List.filterMap getMaybeOut
                |> Json.Encode.object
        )


getMaybeOut : ( Maybe a, b ) -> Maybe ( a, b )
getMaybeOut mb =
    case mb of
        ( Nothing, _ ) ->
            Nothing

        ( Just a, b ) ->
            Just ( a, b )


collapseEncoder : Encoder a -> (a -> Json.Encode.Value)
collapseEncoder enc =
    case enc of
        SingleEncoder s ->
            s

        ListEncoder l ->
            \x -> Json.Encode.object (List.map (\y -> y x) l)


entity : b -> PartialIO b a
entity new =
    { decoder = succeed new
    , toString = \_ _ -> Nothing
    , encoder = ListEncoder []
    }


attribute : String -> IO a -> (c -> a) -> PartialIO (a -> b) c -> PartialIO b c
attribute name def getter parent =
    { decoder =
        parent.decoder
            |> required name def.decoder
    , toString =
        \acc value ->
            let
                ( head, tail ) =
                    parseHeadTail acc
            in
            if name == head then
                def.toString tail (getter value)

            else
                parent.toString acc value
    , encoder =
        case ( parent.encoder, def.encoder ) of
            ( ListEncoder pe, SingleEncoder de ) ->
                ListEncoder <|
                    [ \x -> ( name, de (getter x) ) ]
                        ++ pe

            ( ListEncoder pe, ListEncoder de ) ->
                ListEncoder <|
                    [ \x -> ( name, listToSingle de (getter x) ) ]
                        ++ pe

            ( SingleEncoder pe, _ ) ->
                SingleEncoder pe
    }


reference : String -> IO a -> (c -> Reference a) -> PartialIO (Reference a -> b) c -> PartialIO b c
reference name _ getter parent =
    { decoder =
        parent.decoder
            |> required name (Json.Decode.map (\x -> Reference x) Json.Decode.string)
    , encoder =
        case parent.encoder of
            ListEncoder pe ->
                ListEncoder
                    ((\x ->
                        ( name
                        , case getter x of
                            Reference value ->
                                Json.Encode.string value
                        )
                     )
                        :: pe
                    )

            SingleEncoder pe ->
                SingleEncoder pe
    , toString =
        \acc value ->
            let
                ( head, _ ) =
                    parseHeadTail acc
            in
            if name == head then
                case getter value of
                    Reference r ->
                        Just r

            else
                parent.toString acc value
    }


listToSingle : List (a -> ( String, Json.Encode.Value )) -> (a -> Json.Encode.Value)
listToSingle l =
    \x -> Json.Encode.object (List.map (\y -> y x) l)


map_decoder_maybe : (Decoder (delta -> target) -> Decoder target) -> Decoder (Maybe delta -> target) -> Decoder target
map_decoder_maybe olddecoder newhandle =
    Json.Decode.map map_maybe_func newhandle
        |> olddecoder


map_maybe_func : (Maybe delta -> target) -> delta -> target
map_maybe_func func val =
    func (Just val)



-- Turns 1.stuff["even"].more -> (1, stuff["even"].more)


parseHeadTail : String -> ( String, String )
parseHeadTail accessor =
    let
        index =
            String.split "." accessor
                |> List.head
                |> Maybe.withDefault ""

        rest =
            String.split "." accessor
                |> List.tail
                |> Maybe.map (String.join ".")
                |> Maybe.withDefault ""
    in
    ( index, rest )


map_list_toString : (String -> kind -> Maybe String) -> String -> List kind -> Maybe String
map_list_toString old s l =
    let
        ( head, rest ) =
            parseHeadTail s
    in
    String.toInt head
        |> Maybe.andThen (\x -> List.Extra.getAt x l)
        |> Maybe.andThen (old rest)


map_dict_toString : (String -> Maybe comparable) -> (String -> value -> Maybe String) -> String -> Dict comparable value -> Maybe String
map_dict_toString key_parser value s d =
    let
        ( head, rest ) =
            parseHeadTail s
    in
    key_parser head
        |> Maybe.andThen (\x -> Dict.get x d)
        |> Maybe.andThen (value rest)
