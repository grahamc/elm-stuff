module Main exposing (..)

import Html exposing (node, Attribute, Html, a, text, em, table, tr, th, td, tbody, thead, input, ul, li, button, h1, h2, div, pre, p, hr)
import Html.Attributes exposing (placeholder, src, disabled, href, class, autofocus, value, id)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Encode exposing (encode, Value, null)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Dict exposing (Dict, empty, foldr, filter)
import List
import Navigation
import QueryString
import Debug
import Maybe
import Array
import ExEmElm.Parser
import ExEmElm.Traverse
import List.Extra


main =
    Navigation.program ChangeUrl
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Gson
    = GString String
    | GBool Bool
    | GInt Int
    | GFloat Float
    | GList (List Gson)
    | GDict (Dict String Gson)
    | GNull


type alias Option =
    { declarations : List String
    , default : Gson
    , description : String
    , example : Gson
    , readOnly : Bool
    , datatype : String
    }


type alias NixOptions =
    List (String, Option)


type alias Model =
    { query : String
    , terms : List String
    , options : NixOptions
    , matchingOptions : NixOptions
    , page : Int
    , location : Navigation.Location
    , selected : Maybe String
    }


clamped : Model -> Model
clamped model =
    { model | page = (clamp 1 (total_pages model.matchingOptions) model.page) }

option_sort : (String, Option) -> String
option_sort (name, option) =
  name

update_options : Model -> NixOptions -> Model
update_options model unsorted_options =
    let
        options = List.sortBy option_sort unsorted_options
        matchingOptions =
            filtered options model.terms

        page = case model.selected of
          Just option ->
            case which_page matchingOptions option of
               Just p -> p
               Nothing -> model.page
          Nothing ->
            model.page
    in
        clamped
            ({ model
                | options = options
                , matchingOptions = matchingOptions
                , page = page
             }
            )


update_query : Model -> String -> Model
update_query model query =
    { model
        | query = query
        , terms = splitQuery query
        , matchingOptions = filtered model.options model.terms
        , page = 1
        , selected = Nothing
    }


select_option : Model -> String -> Model
select_option model opt =
    { model
        | selected = Just opt
    }


deselect_option : Model -> Model
deselect_option model =
    { model
        | selected = Nothing
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        qs =
            QueryString.parse location.search

        query =
            Maybe.withDefault "" (QueryString.one QueryString.string "query" qs)

        page =
            Maybe.withDefault 1 (QueryString.one QueryString.int "page" qs)

        selected =
            QueryString.one QueryString.string "selected" qs
    in
        ( Model query (splitQuery query) [] [] page location selected
        , getOptions
        )


splitQuery : String -> List String
splitQuery query =
    String.words (String.toLower query)



-- UPDATE


updateUrl : Model -> Cmd Msg
updateUrl model =
    let
        querystring = case model.selected of
                    Nothing ->
                      String.append
                          (String.append "?query=" (Http.encodeUri model.query))
                          (if model.page > 1 then
                            (String.append "&page=" (Http.encodeUri (toString model.page)))
                            else
                                ""
                          )

                    Just opt ->
                        (String.append "?selected=" (Http.encodeUri opt))


    in Navigation.newUrl querystring



type Msg
    = ChangeQuery String
    | ChangePage Int
    | ChangeUrl Navigation.Location
    | FetchedOptions (Result Http.Error NixOptions)
    | SelectOption String
    | DeselectOption


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedOptions (Ok options) ->
            let
                newModel =
                    update_options model options
            in
                ( newModel, updateUrl newModel )

        FetchedOptions (Err e) ->
            case e of
                Http.BadPayload str _ ->
                    ( { model | query = str }, Cmd.none )

                otherwise ->
                    ( { model | query = ":/" }, Cmd.none )

        SelectOption option_name ->
            let
                newModel =
                    select_option model option_name
            in
                ( newModel, updateUrl newModel )

        DeselectOption ->
            let
                newModel =
                    deselect_option model
            in
                ( newModel, updateUrl newModel )

        ChangeUrl location ->
            ( model, Cmd.none )

        ChangePage newPage ->
            let
                newModel =
                    clamped { model | page = newPage
                                    , selected = Nothing }
            in
                ( newModel, updateUrl newModel )

        ChangeQuery newQuery ->
            let
                newModel =
                    update_query model newQuery
            in
                ( newModel, updateUrl newModel )



-- VIEW


tt : List (Attribute msg) -> List (Html msg) -> Html msg
tt x y =
    node "tt" x y


declaration_link : String -> Html Msg
declaration_link path =
    a [ href (String.append "https://github.com/NixOS/nixpkgs/tree/release-17.03/" path) ]
        [ (tt [] [ text path ]) ]


null_is_not_given : (Gson -> Html Msg) -> Gson -> Html Msg
null_is_not_given fn x =
    if x == GNull then
        (em [] [ text "Not given" ])
    else
        fn x


nix : Gson -> Html Msg
nix content =
    pre [] [ text (nix1 content) ]


nix1 : Gson -> String
nix1 gson =
    case gson of
        GNull ->
            "null"

        GString x ->
            nix_string x

        GBool True ->
            "true"

        GBool False ->
            "false"

        GInt x ->
            toString x

        GFloat x ->
            toString x

        GList list ->
            nix_list list

        GDict dict ->
            if ((Dict.member "_type" dict) && ((Dict.get "_type" dict) == Just (GString "literalExample")) && (Dict.member "text" dict)) then
                case Dict.get "text" dict of
                    Just (GString txt) ->
                        txt

                    otherwise ->
                        "Literal example failed to produce text"
            else
                let
                    entries =
                        Dict.toList dict
                in
                    if List.length entries == 0 then
                        "{ }"
                    else
                        String.concat
                            (List.concat
                                [ [ "{\n" ]
                                , indent
                                    (List.concat
                                        (List.map nix_enc_dict entries)
                                    )
                                , [ "}" ]
                                ]
                            )


nix_string : String -> String
nix_string x =
    if String.contains "\n" x then
        String.concat
            (List.concat
                [ [ "''\n" ]
                , [ x ]
                    |> segments_to_lines
                    |> indent
                , [ "''" ]
                ]
            )
    else
        String.concat [ "\"", x, "\"" ]


nix_list : List Gson -> String
nix_list list =
    if List.length list == 0 then
        "[]"
    else if List.length list == 1 then
        String.concat
            [ "[ "
            , (nix1 (Maybe.withDefault GNull (List.head list)))
            , " ]"
            ]
    else
        String.concat
            (List.concat
                [ [ "[\n" ]
                , indent (List.map (\x -> String.append (nix1 x) "\n") list)
                , [ "]" ]
                ]
            )


indent : List String -> List String
indent lines =
    List.map (\line -> String.append "  " line) lines


nix_enc_dict : ( String, Gson ) -> List String
nix_enc_dict ( key, value ) =
    [ "\""
    , key
    , "\" = "
    , (nix1 value)
    , ";"
    ]
        |> segments_to_lines


segments_to_lines : List String -> List String
segments_to_lines segments =
    segments
        |> String.concat
        |> String.lines
        |> List.map (\x -> String.append x "\n")


description_to_text : String -> String
description_to_text input =
    let
        text =
            String.concat
                [ "<xml xmlns:xlink=\"http://www.w3.org/1999/xlink\"><para>"
                , input
                , "</para></xml>"
                ]
    in
        case ExEmElm.Parser.parse text of
            Ok doc ->
                ExEmElm.Traverse.innerText (ExEmElm.Parser.root doc)

            otherwise ->
                ":)"


describe_option : Option -> Html Msg
describe_option option =
                div [ class "search-details" ] [
    table []
        [ tbody []
            [ tr []
                [ th [] [ text "Description:" ]
                , td [] []
                , td [ class "description docbook" ] [ text (description_to_text option.description) ]
                ]
            , tr []
                [ th [] [ text "Default Value:" ]
                , td [] []
                , td [ class "default" ] [ null_is_not_given nix option.default ]
                ]
            , tr []
                [ th [] [ text "Example value:" ]
                , td [] []
                , td [ class "example" ] [ null_is_not_given nix option.example ]
                ]
            , tr []
                [ th [] [ text "Declared In:" ]
                , td [] []
                , td [ class "declared-in" ] (List.map declaration_link option.declarations)
                ]
            ]
        ]
        ]

optionToTd : Maybe String -> (String, Option) -> List (Html Msg)
optionToTd selected (name, option) =
    let
        isSelected =
            (case selected of
                Nothing ->
                    False

                Just selopt ->
                    selopt == name
            )
    in
        (List.concat
            [ [ (tr []
                    [ td
                        [ onClick
                            (if isSelected then
                                (DeselectOption)
                             else
                                (SelectOption name)
                            )
                        ]
                        [ tt [] [ text name ] ]
                    ]
                )
              ]
            , (if not isSelected then
                []
               else
                [ tr []
                    [ td [ class "details" ] [ describe_option option ]
                    ]
                ]
              )
            ]
        )


term_matches : String -> String -> Bool
term_matches name term =
    String.contains term (String.toLower name)


option_filter : List String -> (String, Option) -> Bool
option_filter terms (name, option) =
    List.any (term_matches name) terms
        || List.any (term_matches option.description) terms


filtered : NixOptions -> List String -> NixOptions
filtered options terms =
    List.filter (option_filter terms) options


opt_tuple_matches_name : String -> (String, Option) -> Bool
opt_tuple_matches_name term (name, option) =
   term == name

which_page : NixOptions -> String -> Maybe Int
which_page options term =
  let
    pos = List.Extra.findIndex (opt_tuple_matches_name term) options
  in case pos of
    Just position -> Just (ceiling ((toFloat position) / 15.0))
    Nothing -> Nothing

fetch_page : Int -> List a -> List a
fetch_page page content =
    List.drop ((page - 1) * 15) (List.take (page * 15) content)


total_pages : NixOptions -> Int
total_pages content =
    ceiling ((toFloat (List.length content)) / 15)


changePageIf : Bool -> Int -> String -> Html Msg
changePageIf cond page txt =
    button
        (if cond then
            [ onClick (ChangePage page) ]
         else
            [ disabled True ]
        )
        [ text txt ]


view : Model -> Html Msg
view model =
    div [ class "container main" ]
        [ div [ class "page-header" ] [ h1 [ ] [ text "Search NixOS options" ] ]
        , p [] [
          input [ id "search", class "search-query span3", autofocus True, value model.query, onInput ChangeQuery ] []
        ]
        , hr [] []
        , p [ id "how-many" ] [ em [] [ text (String.concat
            [ "Showing results "
            , (toString (((model.page - 1) * 15) + 1))
            , "-"
            , (toString (min (List.length model.matchingOptions) ((model.page) * 15)))
            , " of "
            , (toString (List.length model.matchingOptions))
            , "."
            ])]]
        , table [ class "options table table-hover" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Option name" ]
                    ]
                ]
            , tbody []
                (List.concat (List.map (optionToTd model.selected) (fetch_page model.page model.matchingOptions)))
            ]
        , ul [ class "pager" ]
            [ li []
                [ (changePageIf (not (model.page == 1)) 1 "« First")
                ]
            , li []
                [ (changePageIf (model.page > 1) (model.page - 1) "‹ Previous")
                ]
            , li []
                [ (changePageIf (model.page < (total_pages model.matchingOptions)) (model.page + 1) "Next ›")
                ]
            , li []
                [ (changePageIf (model.page < (total_pages model.matchingOptions)) (total_pages model.matchingOptions) "Last »")
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP
--gsonDecodeList : Decode.Decoder Gson
--gsonDecodeList =
--  (Decode.list (Decode.lazy (\_ -> gsonDecoder)))


gsonDecoder : Decode.Decoder Gson
gsonDecoder =
    Decode.oneOf
        [ Decode.null GNull
        , Decode.string |> Decode.map GString
        , Decode.bool |> Decode.map GBool
        , Decode.int |> Decode.map GInt
        , Decode.float |> Decode.map GFloat
        , Decode.list (Decode.lazy (\_ -> gsonDecoder)) |> Decode.map mkGsonList
        , Decode.keyValuePairs (Decode.lazy (\_ -> gsonDecoder)) |> Decode.map mkGsonObject
        ]


mkGsonList : List Gson -> Gson
mkGsonList data =
    GList (List.map (\x -> x) data)


mkGsonObject : List ( String, Gson ) -> Gson
mkGsonObject fields =
    GDict (Dict.fromList fields)


getOptions : Cmd Msg
getOptions =
    Http.send FetchedOptions (Http.get "./options.json" decodeOptions)


optionDecoder : Decode.Decoder Option
optionDecoder =
    decode Option
        |> required "declarations" (Decode.list (Debug.log "hey" Decode.string))
        |> optional "default" (Decode.lazy (\_ -> gsonDecoder)) GNull
        |> required "description" Decode.string
        |> optional "example" (Decode.lazy (\_ -> gsonDecoder)) GNull
        |> required "readOnly" Decode.bool
        |> required "type" Decode.string


decodeOptions : Decode.Decoder NixOptions
decodeOptions =
    (Decode.keyValuePairs <| optionDecoder)
