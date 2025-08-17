module VMTranslator exposing (translateFile)

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


translateFile : String -> List String -> List String
translateFile fileName rawInput =
    rawInput
        |> List.map String.trim
        |> List.filter
            (\line -> not <| String.isEmpty line)
        |> List.filter
            (\line -> not <| String.startsWith "//" line)
        |> List.indexedMap
            (\i line ->
                line
                    |> translateLine i fileName
                    |> List.filter (\l -> String.trim l /= "")
            )
        |> List.foldr (\l acc -> l ++ acc) []


translateLine : Int -> String -> String -> List String
translateLine index fileName line =
    let
        vmCommand : Result (List Parser.DeadEnd) VMCommand
        vmCommand =
            line
                |> Parser.run
                    (oneOf
                        [ functionCallParser
                        , functionDeclarationParser
                        , functionReturnParser
                        , pushParser
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
                    getCpuCommands fileName command index
            in
            case asmCommands of
                firstCommand :: rest ->
                    (firstCommand ++ " " ++ commentLine) :: rest

                [] ->
                    []

        Err _ ->
            let
                message =
                    "!!!!! could not process command: " ++ line
            in
            Debug.log message []


functionCallParser : Parser VMCommand
functionCallParser =
    succeed FunctionCall
        |. keyword "call"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscoreOrPeriod)
        |. spaces
        |= int


functionDeclarationParser : Parser VMCommand
functionDeclarationParser =
    succeed FunctionDeclaration
        |. keyword "function"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscoreOrPeriod)
        |. spaces
        |= int


functionReturnParser : Parser VMCommand
functionReturnParser =
    succeed FunctionReturn
        |. keyword "return"


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
        |= getChompedString (chompWhile charIsAlphaNumorUnderscoreOrPeriod)


ifGotoParser : Parser VMCommand
ifGotoParser =
    succeed IfGoto
        |. keyword "if-goto"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscoreOrPeriod)


gotoParser : Parser VMCommand
gotoParser =
    succeed Goto
        |. keyword "goto"
        |. spaces
        |= getChompedString (chompWhile charIsAlphaNumorUnderscoreOrPeriod)


charIsAlphaNumorUnderscoreOrPeriod : Char -> Bool
charIsAlphaNumorUnderscoreOrPeriod c =
    Char.isAlphaNum c || c == '_' || c == '.'
