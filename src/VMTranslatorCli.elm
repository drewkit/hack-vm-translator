module VMTranslatorCli exposing (run)

import BackendTask
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
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


type TranslationStrategy
    = FilePathSpecified String
    | DirectoryPathSpecified String
    | CurrentDirectoryDefault


getTranslationStrategy : CliOptions -> TranslationStrategy
getTranslationStrategy cliOptions =
    case cliOptions.target of
        Nothing ->
            CurrentDirectoryDefault

        Just path ->
            if String.endsWith ".vm" path then
                FilePathSpecified path

            else
                DirectoryPathSpecified path


getFileNameFromPath : String -> String
getFileNameFromPath str =
    str
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "FileNotFound"


run : Script
run =
    Script.withCliOptions scriptConfig
        (\cliOptions ->
            let
                filePaths : BackendTask.BackendTask error (List String)
                filePaths =
                    case getTranslationStrategy cliOptions of
                        FilePathSpecified path ->
                            [ path ] |> BackendTask.succeed

                        DirectoryPathSpecified path ->
                            let
                                directoryPathEndingWithForwardSlash =
                                    if String.endsWith "/" path then
                                        path

                                    else
                                        path ++ "/"
                            in
                            Glob.fromString <|
                                directoryPathEndingWithForwardSlash
                                    ++ "*.vm"

                        CurrentDirectoryDefault ->
                            Glob.fromString "*.vm"

                outputFilePath : String
                outputFilePath =
                    case getTranslationStrategy cliOptions of
                        FilePathSpecified path ->
                            String.replace ".vm" ".asm" path

                        DirectoryPathSpecified path ->
                            let
                                fileName =
                                    path
                                        |> String.split "/"
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.withDefault "Default"
                                        |> (\str -> str ++ ".asm")
                            in
                            if String.endsWith "/" path then
                                path ++ fileName

                            else
                                path ++ "/" ++ fileName

                        CurrentDirectoryDefault ->
                            "vm_translated.asm"

                fileReads : BackendTask.BackendTask FatalError (List String)
                fileReads =
                    filePaths
                        |> BackendTask.andThen
                            (\files ->
                                files
                                    |> List.map
                                        (\f ->
                                            File.rawFile f
                                                |> BackendTask.allowFatal
                                                |> BackendTask.map
                                                    (\content ->
                                                        -- generate comment code vm file name
                                                        "// " ++ getFileNameFromPath f ++ "\n\n" ++ content
                                                    )
                                        )
                                    |> BackendTask.combine
                            )
            in
            fileReads
                |> BackendTask.map (\strs -> String.join "\n\n" strs)
                |> BackendTask.andThen
                    (\content ->
                        Script.writeFile { path = outputFilePath, body = translate content }
                            |> BackendTask.allowFatal
                    )
        )
