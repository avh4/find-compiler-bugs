module ElmCompiler exposing (main)

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Action.Program
import Fuzz.Action.Task exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
        , Fuzz.constant [ "Foo" ]

        -- , Fuzz.constant [ "Foo", "B" ]
        -- , Fuzz.constant [ "String", "Extra" ]
        , Fuzz.constant [ "Main" ]
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
    String.join "\n"
        [ "module " ++ String.join "." name ++ " exposing (main, value)"
        , "import Html exposing (Html)"
        , def.deps
            |> List.map (\n -> "import " ++ String.join "." n)
            |> String.join "\n"
        , ""
        , "value : String"
        , "value = " ++ toString def.value
        , ""
        , "main : Html msg"
        , "main = "
        , "    Html.text <| "
        , "    String.join \":\""
        , def.deps
            |> List.map (\n -> String.join "." n ++ ".value")
            |> String.join "\n        , "
            |> (++) "        [ "
        , "        ]"
        ]


type File
    = CompiledElm


anyFilename : Fuzzer String
anyFilename =
    Fuzz.oneOf
        [ Fuzz.constant "Main.elm"
        , Fuzz.constant "A.elm"
        , Fuzz.constant "ggg.elm"
        ]


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
        , fuzz = 2
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
                            [ ( "filename", Encode.string (String.join "/" filename ++ ".elm") ) ]
                , test =
                    \filename test ->
                        let
                            output =
                                "index.html"
                        in
                        case Dict.get filename test.elmFiles of
                            Nothing ->
                                PreconditionFailed "elm file doesn't exist"

                            Just _ ->
                                Check <|
                                    \actual ->
                                        case actual of
                                            Err _ ->
                                                Err ("Expected Ok, but got: " ++ toString actual)

                                            Ok response ->
                                                if response.code == 0 then
                                                    -- if String.contains ("Successfully generated " ++ output ++ "\n") response.stdout then
                                                    Ok { test | files = Dict.insert output CompiledElm test.files }
                                                else
                                                    Err ("Compilation failed: " ++ toString response)
                }
            , readAndModify2
                { name = "writeElmFile"
                , arg1 = anyModuleName
                , arg2 = anySourceDef
                , action =
                    \filename def () ->
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
                                                Dict.insert filename def test.elmFiles
                                        }
                                else
                                    Err "actual didn't match"
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
