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
    | FunctionDeclaration String Int
    | FunctionCall String Int
    | FunctionReturn


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
            Debug.log ("SEGMENT NOT LCL/ARG/POINTER/THIS/THAT -- " ++ segToStr seg)
                -1


tempBaseRegister : Int
tempBaseRegister =
    5


getCommentLine : VMCommand -> String
getCommentLine command =
    let
        commentMsg =
            case command of
                BinaryArithmetic op ->
                    binaryOpToStr op

                UnaryArithmetic op ->
                    unaryOpToStr op

                Pop seg i ->
                    "pop " ++ segToStr seg ++ " " ++ String.fromInt i

                Push seg i ->
                    "push " ++ segToStr seg ++ " " ++ String.fromInt i

                Label labelName ->
                    "label " ++ labelName

                IfGoto labelName ->
                    "if-goto " ++ labelName

                Goto labelName ->
                    "goto " ++ labelName

                FunctionCall fName nArgs ->
                    "call " ++ fName ++ " " ++ String.fromInt nArgs

                FunctionDeclaration fName nVars ->
                    "function " ++ fName ++ " " ++ String.fromInt nVars

                FunctionReturn ->
                    "return"
    in
    "// " ++ commentMsg


getCpuCommands : String -> VMCommand -> Int -> List String
getCpuCommands fileName vmCommand index =
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
            , "D=M"
            , "@0"
            , "M=M-1"
            , "@" ++ labelName
            , "D;JNE"
            ]

        Goto labelName ->
            [ "@" ++ labelName
            , "0;JMP"
            ]

        FunctionDeclaration fName nVars ->
            let
                pushLocalVar =
                    [ "@0"
                    , "A=M"
                    , "M=0"
                    , "@0"
                    , "M=M+1"
                    ]

                initVars : Int -> List String
                initVars n =
                    List.range 0 (n - 1)
                        |> List.foldl (\_ rest -> pushLocalVar ++ rest) []
            in
            ("(" ++ fName ++ ")") :: initVars nVars

        FunctionCall fName nArgs ->
            let
                returnLabel =
                    fName ++ "_RETURN_" ++ String.fromInt index

                pushSavedSegment : Segment -> List String
                pushSavedSegment seg =
                    let
                        stackSegment segName =
                            [ "@" ++ (String.fromInt <| getSegmentBaseRegister segName) ++ " // push caller segment: " ++ segToStr segName
                            , "D=M"
                            , "@0"
                            , "A=M"
                            , "M=D"
                            , "@0"
                            , "M=M+1"
                            ]
                    in
                    case seg of
                        Local ->
                            stackSegment seg

                        Argument ->
                            stackSegment seg

                        This ->
                            stackSegment seg

                        That ->
                            stackSegment seg

                        _ ->
                            let
                                msg =
                                    "pushedSavedSegment attempting to process a non 'stack' segment -- "
                                        ++ segToStr seg
                            in
                            Debug.log msg []
            in
            [ "@" ++ returnLabel
            , "D=A"
            , "@0"
            , "A=M"
            , "M=D"
            , "@0"
            , "M=M+1"
            ]
                ++ pushSavedSegment Local
                ++ pushSavedSegment Argument
                ++ pushSavedSegment This
                ++ pushSavedSegment That
                ++ [ "@" ++ String.fromInt (5 + nArgs) ++ " // ARG = SP - (nArgs + 5)"
                   , "D=A"
                   , "@0"
                   , "D=M-D"
                   , "@" ++ String.fromInt (getSegmentBaseRegister Argument)
                   , "M=D"
                   , "@0 // reposition LCL to SP"
                   , "D=M"
                   , "@" ++ String.fromInt (getSegmentBaseRegister Local)
                   , "M=D"
                   , "@" ++ fName ++ " // jump to " ++ fName
                   , "0;JMP // goto " ++ fName
                   , "(" ++ returnLabel ++ ")"
                   ]

        FunctionReturn ->
            let
                restoreCallerSegment seg negativeOffset =
                    [ "@" ++ String.fromInt negativeOffset ++ " // restore segment: " ++ segToStr seg
                    , "D=A"
                    , "@R11"
                    , "A=M-D"
                    , "D=M"
                    , "@" ++ String.fromInt (getSegmentBaseRegister seg)
                    , "M=D"
                    ]
            in
            [ "@" ++ String.fromInt (getSegmentBaseRegister Local) ++ " // endFrame = LCL = R11"
            , "D=M"
            , "@R11"
            , "M=D"
            , "@5 // *(endFrame - 5) = R12 = returnInstructionAddress"
            , "D=D-A"
            , "@R12"
            , "M=D"
            , "A=M"
            , "D=M"
            , "@R12"
            , "M=D"
            , "@0 // *ARG = pop() -- puts the return value at the top of the caller stack"
            , "A=M-1"
            , "D=M"
            , "@" ++ String.fromInt (getSegmentBaseRegister Argument)
            , "A=M"
            , "M=D"
            , "@" ++ String.fromInt (getSegmentBaseRegister Argument) ++ " // SP = ARG + 1 -- reposition stack pointer to caller stack"
            , "D=M"
            , "@0"
            , "M=D+1"
            , ""
            ]
                ++ restoreCallerSegment That 1
                ++ restoreCallerSegment This 2
                ++ restoreCallerSegment Argument 3
                ++ restoreCallerSegment Local 4
                ++ [ "@R12 // jump to return instruction address"
                   , "A=M"
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
                    , "M=D"
                    , "@R14"
                    , "M=D-1"
                    ]

                loadSecondOperandToD =
                    [ "@R13"
                    , "A=M"
                    , "D=M"
                    ]

                decrementStackPointer =
                    [ "@0"
                    , "M=M-1"
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
                    buildBitwiseOperation "M=M-D"

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
                nonPointingSegmentPop addrInstruction =
                    [ "@0"
                    , "M=M-1"
                    , "A=M"
                    , "D=M"
                    , addrInstruction
                    , "M=D"
                    ]

                pointingSegmentPop segName =
                    let
                        segmentBaseRegister =
                            getSegmentBaseRegister segName
                    in
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@" ++ String.fromInt segmentBaseRegister
                    , "D=D+M"
                    , "@R13"
                    , "M=D"
                    , "@0"
                    , "M=M-1"
                    , "A=M"
                    , "D=M"
                    , "@R13"
                    , "A=M"
                    , "M=D"
                    ]
            in
            case seg of
                Constant ->
                    Debug.log "!! Attempt made to Pop from Constant segment (not possible)"
                        []

                Pointer ->
                    let
                        pointerBase =
                            getSegmentBaseRegister Pointer
                    in
                    nonPointingSegmentPop ("@" ++ String.fromInt (pointerBase + i))

                Temp ->
                    nonPointingSegmentPop ("@" ++ String.fromInt (tempBaseRegister + i))

                Static ->
                    nonPointingSegmentPop ("@" ++ fileName ++ "." ++ String.fromInt i)

                Local ->
                    pointingSegmentPop seg

                Argument ->
                    pointingSegmentPop seg

                This ->
                    pointingSegmentPop seg

                That ->
                    pointingSegmentPop seg

        Push seg i ->
            let
                nonPointingSegmentPush addrInstruction =
                    [ addrInstruction
                    , "D=M"
                    , "@0"
                    , "A=M"
                    , "M=D"
                    , "@0"
                    , "M=M+1"
                    ]

                pointingSegmentPush segName =
                    let
                        segmentBaseRegister =
                            getSegmentBaseRegister segName
                    in
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@" ++ String.fromInt segmentBaseRegister
                    , "A=D+M"
                    , "D=M"
                    , "@0"
                    , "A=M"
                    , "M=D"
                    , "@0"
                    , "M=M+1"
                    ]
            in
            case seg of
                Constant ->
                    [ "@" ++ String.fromInt i
                    , "D=A"
                    , "@0"
                    , "A=M"
                    , "M=D"
                    , "@0"
                    , "M=M+1"
                    ]

                Pointer ->
                    let
                        pointerBase =
                            getSegmentBaseRegister Pointer
                    in
                    nonPointingSegmentPush ("@" ++ String.fromInt (pointerBase + i))

                Temp ->
                    nonPointingSegmentPush ("@" ++ String.fromInt (tempBaseRegister + i))

                Static ->
                    nonPointingSegmentPush ("@" ++ fileName ++ "." ++ String.fromInt i)

                Local ->
                    pointingSegmentPush seg

                Argument ->
                    pointingSegmentPush seg

                This ->
                    pointingSegmentPush seg

                That ->
                    pointingSegmentPush seg
