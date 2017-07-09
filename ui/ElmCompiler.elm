module ElmCompiler exposing (main)

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Action.Program
import Fuzz.Action.Task exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Set exposing (Set)
import Task


type alias TestModel =
    { files : Dict String File
    , elmFiles : Dict ModuleName SourceDef
    }


initialModel : TestModel
initialModel =
    { files = Dict.empty
    , elmFiles = Dict.empty
    }


type alias ModuleName =
    List String


anyModuleName : Fuzzer ModuleName
anyModuleName =
    Fuzz.oneOf
        [ Fuzz.constant [ "A" ]
        , Fuzz.constant [ "B" ]
        , Fuzz.constant [ "C" ]

        -- , Fuzz.constant [ "Foo", "B" ]
        -- , Fuzz.constant [ "String", "Extra" ]
        -- , Fuzz.constant [ "Main" ]
        ]


type alias SourceDef =
    { deps : List ModuleName
    , value : String
    }


anySourceDef : Fuzzer SourceDef
anySourceDef =
    Fuzz.map2 SourceDef
        (Fuzz.list anyModuleName)
        Fuzz.string


elmFile : ModuleName -> SourceDef -> String
elmFile name def =
    let
        deps =
            def.deps
                |> Set.fromList
                |> Set.remove name
                |> Set.toList
    in
    String.join "\n"
        [ "module " ++ String.join "." name ++ " exposing (main, value)"
        , "import Html exposing (Html)"
        , deps
            |> List.map (\n -> "import " ++ String.join "." n)
            |> String.join "\n"
        , ""
        , "value : String"
        , "value = " ++ toString def.value
        , ""
        , "main : Html msg"
        , "main = "
        , "    Html.text <| "
        , "    Debug.log \"" ++ String.join "." name ++ "\" <|"
        , "    String.join \":\""
        , deps
            |> List.map (\n -> String.join "." n ++ ".value")
            |> (::) "value"
            |> String.join "\n        , "
            |> (++) "        [ "
        , "        ]"
        ]


type File
    = CompiledElm String


type alias Response =
    { code : Int
    , stdout : String
    , stderr : String
    }


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map3 Response
        (Decode.field "code" Decode.int)
        (Decode.field "stdout" Decode.string)
        (Decode.field "stderr" Decode.string)


main =
    Fuzz.Action.Program.program
        { seed = 1
        , fuzz = 10
        , real =
            post "reset" []
                |> Task.andThen
                    (\result ->
                        case result of
                            ( Ok _, () ) ->
                                Task.succeed ()

                            ( Err _, () ) ->
                                Task.fail "reset failed"
                    )
        , test = initialModel
        , actions =
            [ readAndModify1
                { name = "compile"
                , arg = anyModuleName
                , action =
                    \filename () ->
                        post "compile"
                            [ ( "filename", Encode.string (String.join "/" filename ++ ".elm") )
                            , ( "output"
                              , Encode.string <|
                                    if filename == [ "A" ] then
                                        "elm.js"
                                    else
                                        "other.js"
                              )
                            ]
                            |> -- Sleep here to work around https://github.com/elm-lang/elm-make/issues/172
                               Task.andThen (\x -> Process.sleep 1000 |> Task.map (always x))
                , test =
                    \filename test ->
                        let
                            output =
                                if filename == [ "A" ] then
                                    "elm.js"
                                else
                                    "other.js"

                            verifyGraph _ _ =
                                if
                                    List.all (flip Dict.member test.elmFiles)
                                        [ [ "A" ], [ "B" ], [ "C" ] ]
                                then
                                    Ok ()
                                else
                                    Err ("Missing files: " ++ toString (Dict.keys test.elmFiles))

                            -- verifyGraph : ModuleName -> Set ModuleName -> Result String ()
                            -- verifyGraph f seen =
                            --     if Set.member f seen then
                            --         Err "Circular dependency in Elm files"
                            --     else
                            --         case Dict.get f test.elmFiles of
                            --             Nothing ->
                            --                 Err ("elm file doesn't exist: " ++ toString f)
                            --             Just def ->
                            --                 def.deps
                            --                     |> List.foldl (\dep r -> Result.andThen (always <| verifyGraph dep (Set.insert f seen)) r) (Ok ())
                        in
                        case verifyGraph filename Set.empty of
                            Err reason ->
                                PreconditionFailed reason

                            Ok () ->
                                Check <|
                                    \actual ->
                                        case actual of
                                            Err _ ->
                                                Err ("Expected Ok, but got: " ++ toString actual)

                                            Ok response ->
                                                if response.code == 0 then
                                                    -- if String.contains ("Successfully generated " ++ output ++ "\n") response.stdout then
                                                    case [ "A", "B", "C" ] |> List.map (List.singleton >> flip Dict.get test.elmFiles >> Maybe.map .value) of
                                                        [ Just a, Just b, Just c ] ->
                                                            let
                                                                exp =
                                                                    String.join ""
                                                                        [ "C: \"" ++ c ++ "\"\n"
                                                                        , "B: \"" ++ b ++ ":" ++ c ++ "\"\n"
                                                                        , "A: \"" ++ a ++ ":" ++ b ++ ":" ++ c ++ "\"\n"
                                                                        ]
                                                            in
                                                            Ok { test | files = Dict.insert output (CompiledElm exp) test.files }

                                                        _ ->
                                                            Ok test
                                                else
                                                    Err ("Compilation failed: " ++ toString response)
                }
            , readAndModify2
                { name = "changeElmValue"
                , arg1 = anyModuleName
                , arg2 =
                    Fuzz.oneOf (List.map Fuzz.constant [ "abc", "def", "ghi", "jkl", "mno", "pqr", "stu", "vwx", "zy_" ])
                        |> Fuzz.map
                            (\value filename ->
                                case filename of
                                    [ "A" ] ->
                                        { deps = [ [ "B" ], [ "C" ] ]
                                        , value = value
                                        }

                                    [ "B" ] ->
                                        { deps = [ [ "C" ] ]
                                        , value = value
                                        }

                                    [ "C" ] ->
                                        { deps = []
                                        , value = value
                                        }

                                    _ ->
                                        { deps = []
                                        , value = value
                                        }
                            )
                , action =
                    \filename value () ->
                        let
                            def =
                                value filename
                        in
                        post "writeElmFile"
                            [ ( "filename", Encode.string (String.join "/" filename ++ ".elm") )
                            , ( "content", Encode.string (elmFile filename def) )
                            ]
                , test =
                    \filename def test ->
                        Check <|
                            \actual ->
                                if actual == Ok { code = 0, stdout = "", stderr = "" } then
                                    Ok
                                        { test
                                            | elmFiles =
                                                Dict.insert filename (def filename) test.elmFiles
                                        }
                                else
                                    Err "actual didn't match"
                }
            , readAndModify0
                { name = "eval"
                , action =
                    \() ->
                        post "eval"
                            [ ( "filename", Encode.string "elm.js" )
                            ]
                , test =
                    \test ->
                        case [ "A", "B", "C" ] |> List.map (List.singleton >> flip Dict.get test.elmFiles >> Maybe.map .value) of
                            [ Just a, Just b, Just c ] ->
                                case Dict.get "elm.js" test.files of
                                    Just (CompiledElm exp) ->
                                        Check <|
                                            \actual ->
                                                case actual of
                                                    Err _ ->
                                                        Err ("Expected Ok, but got: " ++ toString actual)

                                                    Ok response ->
                                                        if response.code /= 0 then
                                                            Err ("Compilation failed: " ++ toString response)
                                                        else if response.stdout /= exp then
                                                            Err ("expected " ++ exp ++ ", got: " ++ response.stdout)
                                                        else
                                                            Ok test

                                    _ ->
                                        PreconditionFailed "elm.js isn't compiled yet"

                            _ ->
                                PreconditionFailed ("Missing elm files: " ++ toString (Dict.keys test.elmFiles))
                }
            ]
        }


post :
    String
    -> List ( String, Encode.Value )
    -> Task.Task never ( Result Http.Error Response, () )
post path json =
    -- Http.post ("http://localhost:42771/" ++ path)
    Http.post ("http://localhost:8080/" ++ path)
        (Http.jsonBody (Encode.object json))
        responseDecoder
        |> Http.toTask
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.map (flip (,) ())


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)
