module VMTranslatorCli exposing (run)

import BackendTask
import BackendTask.File as File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Pages.Script as Script exposing (Script)
import VMTranslator exposing (translate)


type alias CliOptions =
    { target : Maybe String }


scriptConfig : Program.Config CliOptions
scriptConfig =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.withOptionalPositionalArg
                    (Option.optionalPositionalArg "target")
            )



-- |> OptionsParser.withDoc "Provide a filename or directory to translate Hack VM commands to Hack assembly code"


run : Script
run =
    Script.withCliOptions scriptConfig
        (\{ target } ->
            let
                files : List String
                files =
                    case target of
                        Nothing ->
                            []

                        Just val ->
                            if String.endsWith ".vm" val then
                                String.split ".vm" val
                                    |> List.head
                                    |> Maybe.withDefault "outputFile"
                                    |> (\nameWithoutExtension -> nameWithoutExtension ++ ".asm")
                                    |> (\r -> [ r ])

                            else
                                -- process directory
                                []

                filename : String
                filename =
                    List.head files
                        |> Maybe.withDefault "filename"
            in
            File.rawFile filename
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\file ->
                        let
                            output : String
                            output =
                                translate file
                        in
                        Script.writeFile
                            { path = filename, body = output }
                            |> BackendTask.allowFatal
                    )
        )
