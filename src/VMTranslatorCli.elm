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

        Just rawPath ->
            let
                path =
                    String.trim rawPath
            in
            if String.endsWith ".vm" path then
                FilePathSpecified path

            else
                DirectoryPathSpecified path


run : Script
run =
    Script.withCliOptions scriptConfig
        (\cliOptions ->
            let
                translationStrategy : TranslationStrategy
                translationStrategy =
                    getTranslationStrategy cliOptions

                filePaths : BackendTask.BackendTask error (List String)
                filePaths =
                    case translationStrategy of
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
                    case translationStrategy of
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
                                        )
                                    |> BackendTask.combine
                            )

                headers =
                    case translationStrategy of
                        FilePathSpecified _ ->
                            []

                        _ ->
                            [ "@256 // Bootstrapping Sys.init"
                            , "D=A"
                            , "@0"
                            , "M=D"
                            ]
                                ++ translate [ "call Sys.init 0" ]
            in
            fileReads
                |> BackendTask.map
                    (\fileStrs ->
                        fileStrs
                            |> List.map
                                -- a lot of work to simply flatten lists of lists
                                (\fileStr ->
                                    fileStr
                                        |> String.split "\n"
                                        |> List.foldr (\a b -> a :: b) []
                                )
                            |> List.foldl (\a b -> a ++ b) []
                    )
                |> BackendTask.andThen
                    (\content ->
                        let
                            decorateAssemblyWithROMAddr : Int -> List String -> List String
                            decorateAssemblyWithROMAddr index asmList =
                                case asmList of
                                    [] ->
                                        []

                                    asmInstruction :: rest ->
                                        if String.startsWith "(" asmInstruction then
                                            -- don't increment index for assembly label instructions
                                            asmInstruction :: decorateAssemblyWithROMAddr index rest

                                        else
                                            let
                                                decoration =
                                                    String.padRight 40 ' ' asmInstruction ++ (" // ROM ADDR " ++ String.fromInt index)
                                            in
                                            decoration :: decorateAssemblyWithROMAddr (index + 1) rest

                            romAddrDecoratedAssembly =
                                decorateAssemblyWithROMAddr 0 (headers ++ translate content)
                        in
                        Script.writeFile
                            { path = outputFilePath
                            , body = String.join "\n" romAddrDecoratedAssembly
                            }
                            |> BackendTask.allowFatal
                    )
        )
