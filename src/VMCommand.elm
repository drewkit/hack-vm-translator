module VMCommand exposing
    ( BinaryArithmeticCmd(..)
    , Segment(..)
    , UnaryArithmeticCmd(..)
    , VMCommand(..)
    , getCommentLine
    , getCpuCommands
    , segToStr
    )


type Segment
    = Local
    | Argument
    | Constant
    | Static
    | Temp
    | This
    | That
    | Pointer


type UnaryArithmeticCmd
    = Neg
    | Not


type BinaryArithmeticCmd
    = Add
    | Sub
    | Eq
    | Or
    | Gt
    | Lt
    | And


type VMCommand
    = Push Segment Int
    | Pop Segment Int
    | UnaryArithmetic UnaryArithmeticCmd
    | BinaryArithmetic BinaryArithmeticCmd
    | Label String
    | IfGoto String
    | Goto String


segToStr : Segment -> String
segToStr seg =
    case seg of
        Static ->
            "static"

        Local ->
            "local"

        Argument ->
            "argument"

        Constant ->
            "constant"

        Temp ->
            "temp"

        This ->
            "this"

        That ->
            "that"

        Pointer ->
            "pointer"


binaryOpToStr : BinaryArithmeticCmd -> String
binaryOpToStr op =
    case op of
        Add ->
            "add"

        Sub ->
            "sub"

        Eq ->
            "eq"

        Or ->
            "or"

        And ->
            "and"

        Gt ->
            "gt"

        Lt ->
            "lt"


unaryOpToStr : UnaryArithmeticCmd -> String
unaryOpToStr op =
    case op of
        Neg ->
            "neg"

        Not ->
            "not"


getSegmentBaseRegister : Segment -> Int
getSegmentBaseRegister seg =
    case seg of
        Local ->
            1

        Argument ->
            2

        Pointer ->
            getSegmentBaseRegister This

        This ->
            3

        That ->
            4

        _ ->
            -1


staticBaseRegister : Int
staticBaseRegister =
    16


tempBaseRegister : Int
tempBaseRegister =
    5


getCommentLine : VMCommand -> String
getCommentLine command =
    let
        commentCode =
            "// "
    in
    case command of
        BinaryArithmetic op ->
            commentCode ++ binaryOpToStr op

        UnaryArithmetic op ->
            commentCode ++ unaryOpToStr op

        Pop seg i ->
            commentCode ++ "pop " ++ segToStr seg ++ " " ++ String.fromInt i

        Push seg i ->
            commentCode ++ "push " ++ segToStr seg ++ " " ++ String.fromInt i

        Label labelName ->
            commentCode ++ "label " ++ labelName

        IfGoto labelName ->
            commentCode ++ "if-goto " ++ labelName

        Goto labelName ->
            commentCode ++ "goto " ++ labelName


getCpuCommands : VMCommand -> Int -> List String
getCpuCommands vmCommand index =
    case vmCommand of
        UnaryArithmetic op ->
            case op of
                Neg ->
                    [ "@0"
                    , "A=M-1"
                    , "M=-M"
                    ]

                Not ->
                    [ "@0"
                    , "A=M-1"
                    , "M=!M"
                    ]

        Label labelName ->
            [ "(" ++ labelName ++ ")" ]

        IfGoto labelName ->
            [ "@0"
            , "A=M-1"
            , "D=M // D = *(SP - 1)"
            , "@0"
            , "M=M-1 // SP--"
            , "@" ++ labelName
            , "D;JGT"
            ]

        Goto labelName ->
            [ "@" ++ labelName
            , "0;JMP"
            ]

        BinaryArithmetic op ->
            -- first operand is stored to R14
            -- second operand is stored to R13
            let
                labelDeclaration labelName =
                    "(" ++ labelName ++ "_" ++ String.fromInt index ++ ")"

                labelReference labelName =
                    "@" ++ labelName ++ "_" ++ String.fromInt index

                initOperands =
                    [ "@0"
                    , "D=M-1"
                    , "@R13"
                    , "M=D // R13 = (SP - 1)"
                    , "@R14"
                    , "M=D-1 // R14 = (SP - 2)"
                    ]

                loadSecondOperandToD =
                    [ "@R13"
                    , "A=M"
                    , "D=M // D = *R13"
                    ]

                decrementStackPointer =
                    [ "@0"
                    , "M=M-1 // SP--"
                    ]

                buildBitwiseOperation operation =
                    initOperands
                        ++ loadSecondOperandToD
                        ++ [ "@R14"
                           , "A=M"
                           , operation
                           ]
                        ++ decrementStackPointer

                buildComparisonOperation comparator =
                    initOperands
                        ++ loadSecondOperandToD
                        ++ [ "@R14"
                           , "A=M"
                           , "D=M-D"
                           , labelReference comparator -- e.g., "@LABEL_NAME"
                           , "D;J" ++ comparator -- e.g., "D;JGT"
                           , "@R14"
                           , "A=M"
                           , "M=0"
                           , labelReference "CONT"
                           , "0;JMP"
                           , labelDeclaration comparator -- e.g., "(LABEL_NAME)"
                           , "@R14"
                           , "A=M"
                           , "M=-1"
                           , labelDeclaration "CONT"
                           ]
                        ++ decrementStackPointer
            in
            case op of
                Add ->
                    buildBitwiseOperation "M=D+M"

                Sub ->
                    buildBitwiseOperation "M=M-D // *(SP - 2) = *(SP - 2) - *(SP - 1)"

                Or ->
                    buildBitwiseOperation "M=D|M"

                And ->
                    buildBitwiseOperation "M=D&M"

                Eq ->
                    buildComparisonOperation "EQ"

                Gt ->
                    buildComparisonOperation "GT"

                Lt ->
                    buildComparisonOperation "LT"

        Pop seg i ->
            let
                nonPointingSegmentPop segmentBase =
                    [ "@" ++ String.fromInt segmentBase
                    , "D=A"
                    , "@" ++ String.fromInt i
                    , "D=D+A"
                    , "@R13"
                    , "M=D // R13 = (base + i)"
                    , "@0"
                    , "A=M-1"
                    , "D=M // *(SP - 1) = D"
                    , "@R13"
                    , "A=M"
                    , "M=D // *R13 = D, which is equivalent to *(base + i) = D"
                    , "@0"
                    , "M=M-1 // SP--"
                    ]

                pointingSegmentPop =
                    let
                        segmentBaseRegister =
                            getSegmentBaseRegister seg
                    in
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@" ++ String.fromInt segmentBaseRegister
                    , "D=D+M"
                    , "@R13"
                    , "M=D // R13 = (base + i)"
                    , "@0"
                    , "M=M-1 // SP--"
                    , "A=M"
                    , "D=M"
                    , "@R13"
                    , "A=M"
                    , "M=D // *R13 = *SP, which is equivalent to *(base + i) = *SP"
                    ]
            in
            case seg of
                Constant ->
                    []

                Pointer ->
                    let
                        pointerBase =
                            getSegmentBaseRegister Pointer
                    in
                    nonPointingSegmentPop pointerBase

                Static ->
                    nonPointingSegmentPop staticBaseRegister

                Temp ->
                    nonPointingSegmentPop tempBaseRegister

                _ ->
                    pointingSegmentPop

        Push seg i ->
            let
                nonPointingSegmentPush segmentBase =
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@" ++ String.fromInt segmentBase
                    , "D=D+A"
                    , "A=D"
                    , "D=M // D = *(base + i)"
                    , "@0"
                    , "A=M"
                    , "M=D // *SP = D"
                    , "@0"
                    , "M=M+1 // SP++"
                    ]

                pointingSegmentPush =
                    let
                        segmentBaseRegister =
                            getSegmentBaseRegister seg
                    in
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@" ++ String.fromInt segmentBaseRegister
                    , "A=D+M // A = (base + i)"
                    , "D=M // D = *(base + i)"
                    , "@0"
                    , "A=M"
                    , "M=D // *SP = D"
                    , "@0"
                    , "M=M+1 // SP++"
                    ]
            in
            case seg of
                Constant ->
                    [ "@" ++ String.fromInt i
                    , "D=A // D = i"
                    , "@0"
                    , "A=M"
                    , "M=D // *SP = D"
                    , "@0"
                    , "M=M+1 // SP++"
                    ]

                Pointer ->
                    let
                        pointerBase =
                            getSegmentBaseRegister Pointer

                        pointerRegister =
                            case i of
                                0 ->
                                    pointerBase

                                1 ->
                                    pointerBase + 1

                                _ ->
                                    pointerBase
                    in
                    nonPointingSegmentPush pointerRegister

                Static ->
                    nonPointingSegmentPush staticBaseRegister

                Temp ->
                    nonPointingSegmentPush tempBaseRegister

                _ ->
                    pointingSegmentPush
