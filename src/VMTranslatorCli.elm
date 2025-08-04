module VMTranslatorCli exposing (run)

import BackendTask
import BackendTask.File as File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Pages.Script as Script exposing (Script)
import VMTranslator exposing (translate)


type alias CliOptions =
    { filename : String }


scriptConfig : Program.Config CliOptions
scriptConfig =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "filename")
                |> OptionsParser.end
                |> OptionsParser.withDoc "Provide a filename to translate Hack VM commands to Hack assembly code"
            )


run : Script
run =
    Script.withCliOptions scriptConfig
        (\{ filename } ->
            File.rawFile filename
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\file ->
                        let
                            outputFilename : String
                            outputFilename =
                                String.split ".vm" filename
                                    |> List.head
                                    |> Maybe.withDefault "outputFile"
                                    |> (\nameWithoutExtension -> nameWithoutExtension ++ ".asm")

                            output : String
                            output =
                                translate file
                        in
                        Script.writeFile
                            { path = outputFilename, body = output }
                            |> BackendTask.allowFatal
                    )
        )
