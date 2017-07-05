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
    }


type File
    = ElmSource
    | CompiledElm


initialModel : TestModel
initialModel =
    { files = Dict.empty
    }


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
                , pre =
                    \filename test ->
                        case Dict.get filename test.files of
                            Just ElmSource ->
                                Ok ()

                            Nothing ->
                                Err "file doesn't exist"

                            Just _ ->
                                Err "file isn't an Elm source file"
                , arg = anyFilename
                , action =
                    \filename () ->
                        post "compile"
                            [ ( "filename", Encode.string filename ) ]
                , test =
                    \filename test ->
                        ( Ok { code = 0, stdout = "", stderr = "" }
                        , { test | files = Dict.insert filename CompiledElm test.files }
                        )
                }
            , readAndModify1
                { name = "writeElmFile"
                , pre = \filename test -> Ok ()
                , arg = anyFilename
                , action =
                    \filename () ->
                        post "writeElmFile"
                            [ ( "filename", Encode.string filename ) ]
                , test =
                    \filename test ->
                        ( Ok { code = 0, stdout = "", stderr = "" }
                        , { test | files = Dict.insert filename ElmSource test.files }
                        )
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
