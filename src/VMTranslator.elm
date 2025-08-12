module VMTranslator exposing (translate)

import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, int, keyword, map, oneOf, spaces, succeed)
import VMCommand
    exposing
        ( BinaryArithmeticCmd(..)
        , Segment(..)
        , UnaryArithmeticCmd(..)
        , VMCommand(..)
        , getCommentLine
        , getCpuCommands
        , segToStr
        )


translate : String -> String
translate rawInput =
    rawInput
        |> String.trim
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter
            (\line -> not <| String.isEmpty line)
        |> List.filter
            (\line -> not <| String.startsWith "//" line)
        |> List.indexedMap
            (\i line ->
                line
                    |> translateLine i
                    |> String.join "\n"
                    |> (\l -> l ++ "\n")
            )
        |> String.join "\n"
        |> (++) "\n"


translateLine : Int -> String -> List String
translateLine index line =
    let
        vmCommand : Result (List Parser.DeadEnd) VMCommand
        vmCommand =
            line
                |> Parser.run
                    (oneOf
                        [ pushParser
                        , popParser
                        , addParser
                        , subParser
                        , negParser
                        , eqParser
                        , gtParser
                        , ltParser
                        , andParser
                        , orParser
                        , notParser
                        , labelParser
                        , ifGotoParser
                        , gotoParser
                        ]
                    )
    in
    case vmCommand of
        Ok command ->
            let
                commentLine =
                    getCommentLine command

                asmCommands =
                    getCpuCommands command index
            in
            commentLine :: asmCommands

        Err _ ->
            [ "// !!!!! could not process line: " ++ line ]


pushParser : Parser VMCommand
pushParser =
    succeed Push
        |. keyword "push"
        |. spaces
        |= segmentParser
        |. spaces
        |= int


popParser : Parser VMCommand
popParser =
    succeed Pop
        |. keyword "pop"
        |. spaces
        |= segmentParser
        |. spaces
        |= int


segmentParser : Parser Segment
segmentParser =
    oneOf
        [ keyword (segToStr Static)
            |> map (\_ -> Static)
        , keyword (segToStr Local)
            |> map (\_ -> Local)
        , keyword (segToStr Constant)
            |> map (\_ -> Constant)
        , keyword (segToStr Argument)
            |> map (\_ -> Argument)
        , keyword (segToStr Temp)
            |> map (\_ -> Temp)
        , keyword (segToStr This)
            |> map (\_ -> This)
        , keyword (segToStr That)
            |> map (\_ -> That)
        , keyword (segToStr Pointer)
            |> map (\_ -> Pointer)
        ]


addParser : Parser VMCommand
addParser =
    succeed (BinaryArithmetic Add)
        |. keyword "add"


subParser : Parser VMCommand
subParser =
    succeed (BinaryArithmetic Sub)
        |. keyword "sub"


negParser : Parser VMCommand
negParser =
    succeed (UnaryArithmetic Neg)
        |. keyword "neg"


eqParser : Parser VMCommand
eqParser =
    succeed (BinaryArithmetic Eq)
        |. keyword "eq"


gtParser : Parser VMCommand
gtParser =
    succeed (BinaryArithmetic Gt)
        |. keyword "gt"


ltParser : Parser VMCommand
ltParser =
    succeed (BinaryArithmetic Lt)
        |. keyword "lt"


andParser : Parser VMCommand
andParser =
    succeed (BinaryArithmetic And)
        |. keyword "and"


orParser : Parser VMCommand
orParser =
    succeed (BinaryArithmetic Or)
        |. keyword "or"


notParser : Parser VMCommand
notParser =
    succeed (UnaryArithmetic Not)
        |. keyword "not"


labelParser : Parser VMCommand
labelParser =
    succeed Label
        |. keyword "label"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscore)


ifGotoParser : Parser VMCommand
ifGotoParser =
    succeed IfGoto
        |. keyword "if-goto"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscore)


gotoParser : Parser VMCommand
gotoParser =
    succeed Goto
        |. keyword "goto"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscore)


charIsAlphaNumorUnderscore : Char -> Bool
charIsAlphaNumorUnderscore c =
    Char.isAlphaNum c || c == '_'
