// Bootstrapping Sys.init
@256
D=A
@0
M=D // SP = 256
@Sys.init
0;JMP // call Sys.init (no arguments)
// Main.vm

// This file is part of www.nand2tetris.org

// and the book "The Elements of Computing Systems"

// by Nisan and Schocken, MIT Press.

// File name: projects/8/FunctionCalls/FibonacciElement/Main.vm

// Contains one function: Main.fibonacci.

// Computes the n'th element of the Fibonacci series, recursively.

// n is given in argument[0]. Called by the Sys.init function

// (part of the Sys.vm file), which sets argument[0] to an input

// value and then calls Main.fibonacci.

// function Main.fibonacci 0
(Main.fibonacci)

// push argument 0
@0
D=A
@2
A=D+M // A = (base + i)
D=M // D = *(base + i)
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// push constant 2
@2
D=A // D = i
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// lt
@0
D=M-1
@R13
M=D // R13 = (SP - 1)
@R14
M=D-1 // R14 = (SP - 2)
@R13
A=M
D=M // D = *R13
@R14
A=M
D=M-D
@LT_13
D;JLT
@R14
A=M
M=0
@CONT_13
0;JMP
(LT_13)
@R14
A=M
M=-1
(CONT_13)
@0
M=M-1 // SP--

// if-goto N_LT_2
@0
A=M-1
D=M // D = *(SP - 1)
@0
M=M-1 // SP--
@N_LT_2
D;JNE

// goto N_GE_2
@N_GE_2
0;JMP

// label N_LT_2
(N_LT_2)

// push argument 0
@0
D=A
@2
A=D+M // A = (base + i)
D=M // D = *(base + i)
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// return
@1
D=M
@R13
M=D // endFrame = LCL = R13
@5
D=M-A
A=M
D=M
@R14
M=D // *(endFrame - 5) = R14 = returnInstructionAddress
@0
A=M-1
D=M
@2
A=M
M=D // *ARG = pop() -- puts the return value at the top of the caller stack
@2
D=M
@0
M=D+1 // SP = ARG + 1 -- reposition stack pointer to caller stack

@1
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@4
M=D // SEG = *(endFrame - offset)
@2
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@3
M=D // SEG = *(endFrame - offset)
@3
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@2
M=D // SEG = *(endFrame - offset)
@4
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@1
M=D // SEG = *(endFrame - offset)
@R14
A=M
0;JMP // jump to return instruction address

// label N_GE_2
(N_GE_2)

// push argument 0
@0
D=A
@2
A=D+M // A = (base + i)
D=M // D = *(base + i)
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// push constant 2
@2
D=A // D = i
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// sub
@0
D=M-1
@R13
M=D // R13 = (SP - 1)
@R14
M=D-1 // R14 = (SP - 2)
@R13
A=M
D=M // D = *R13
@R14
A=M
M=M-D // *(SP - 2) = *(SP - 2) - *(SP - 1)
@0
M=M-1 // SP--

// call Main.fibonacci 1
@Main.fibonacci.RETURN.23
D=A
@0
A=M
M=D // return address pushed to stack
@0
M=M+1 // SP++
@1
A=M
D=M
@0
A=M
M=D // pushed caller local segment to stack
@0
M=M+1 // SP++
@2
A=M
D=M
@0
A=M
M=D // pushed caller argument segment to stack
@0
M=M+1 // SP++
@3
A=M
D=M
@0
A=M
M=D // pushed caller this segment to stack
@0
M=M+1 // SP++
@4
A=M
D=M
@0
A=M
M=D // pushed caller that segment to stack
@0
M=M+1 // SP++
@0
A=M
D=M
@1
M=D // LCL = SP
@6
D=D-A
@2
M=D // ARG = SP - 5 - nArgs
@Main.fibonacci
0;JMP
(Main.fibonacci.RETURN.23)

// push argument 0
@0
D=A
@2
A=D+M // A = (base + i)
D=M // D = *(base + i)
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// push constant 1
@1
D=A // D = i
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// sub
@0
D=M-1
@R13
M=D // R13 = (SP - 1)
@R14
M=D-1 // R14 = (SP - 2)
@R13
A=M
D=M // D = *R13
@R14
A=M
M=M-D // *(SP - 2) = *(SP - 2) - *(SP - 1)
@0
M=M-1 // SP--

// call Main.fibonacci 1
@Main.fibonacci.RETURN.27
D=A
@0
A=M
M=D // return address pushed to stack
@0
M=M+1 // SP++
@1
A=M
D=M
@0
A=M
M=D // pushed caller local segment to stack
@0
M=M+1 // SP++
@2
A=M
D=M
@0
A=M
M=D // pushed caller argument segment to stack
@0
M=M+1 // SP++
@3
A=M
D=M
@0
A=M
M=D // pushed caller this segment to stack
@0
M=M+1 // SP++
@4
A=M
D=M
@0
A=M
M=D // pushed caller that segment to stack
@0
M=M+1 // SP++
@0
A=M
D=M
@1
M=D // LCL = SP
@6
D=D-A
@2
M=D // ARG = SP - 5 - nArgs
@Main.fibonacci
0;JMP
(Main.fibonacci.RETURN.27)

// add
@0
D=M-1
@R13
M=D // R13 = (SP - 1)
@R14
M=D-1 // R14 = (SP - 2)
@R13
A=M
D=M // D = *R13
@R14
A=M
M=D+M
@0
M=M-1 // SP--

// return
@1
D=M
@R13
M=D // endFrame = LCL = R13
@5
D=M-A
A=M
D=M
@R14
M=D // *(endFrame - 5) = R14 = returnInstructionAddress
@0
A=M-1
D=M
@2
A=M
M=D // *ARG = pop() -- puts the return value at the top of the caller stack
@2
D=M
@0
M=D+1 // SP = ARG + 1 -- reposition stack pointer to caller stack

@1
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@4
M=D // SEG = *(endFrame - offset)
@2
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@3
M=D // SEG = *(endFrame - offset)
@3
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@2
M=D // SEG = *(endFrame - offset)
@4
D=A
@R13
A=M-D
D=M // D = *(endFrame - offset)
@1
M=D // SEG = *(endFrame - offset)
@R14
A=M
0;JMP // jump to return instruction address

// Sys.vm

// This file is part of www.nand2tetris.org

// and the book "The Elements of Computing Systems"

// by Nisan and Schocken, MIT Press.

// File name: projects/8/FunctionCalls/FibonacciElement/Sys.vm

// Containts one function: Sys.init.

// Pushes a constant n onto the stack, and calls the Main.fibonacii

// function, which computes the n'th element of the Fibonacci series.

// Note that by convention, the Sys.init function is called "automatically"

// by the bootstrap code generated by the VM translator.

// function Sys.init 0
(Sys.init)

// Computes fibonacci(4)

// push constant 4
@4
D=A // D = i
@0
A=M
M=D // *SP = D
@0
M=M+1 // SP++

// Calls the function, informing that one argument was pushed onto the stack

// call Main.fibonacci 1
@Main.fibonacci.RETURN.44
D=A
@0
A=M
M=D // return address pushed to stack
@0
M=M+1 // SP++
@1
A=M
D=M
@0
A=M
M=D // pushed caller local segment to stack
@0
M=M+1 // SP++
@2
A=M
D=M
@0
A=M
M=D // pushed caller argument segment to stack
@0
M=M+1 // SP++
@3
A=M
D=M
@0
A=M
M=D // pushed caller this segment to stack
@0
M=M+1 // SP++
@4
A=M
D=M
@0
A=M
M=D // pushed caller that segment to stack
@0
M=M+1 // SP++
@0
A=M
D=M
@1
M=D // LCL = SP
@6
D=D-A
@2
M=D // ARG = SP - 5 - nArgs
@Main.fibonacci
0;JMP
(Main.fibonacci.RETURN.44)

// label END
(END)

// goto END
@END
0;JMP
