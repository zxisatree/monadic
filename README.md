# MonadIC

## Overview

MonadIC is both a compiler and interpreter for an imperative language, written in Haskell. Programs are written in Haskell, using custom data types to emulate keywords. The interpreter starts a program with an empty memory, and when it finishes execution, it returns the memory of the program that it completed (an example is given in [Usage](#usage)).

Programs are represented using the data type `Prog`. A program consists is either a sequence of instructions, a variable assignment, an if statement, or a while statement. The list of instructions (`Inst`) are:

- PUSH: push to top of stack
- PUSHV: push value of variable to top of stack
- POP: pop top of stack and assign it to variable
- DO: remove top 2 values from stack, operate on it, push result to stack
- JUMP: jump to label
- JUMPZ: conditional jump, pops top of stack, if value is 0, then jump, else don't
- LABEL: int labelling a place in code, for use in JUMP instructions

## Usage

First, ensure that you have Haskell installed on your system. Then run `ghci monadic.hs` to start the interpreter. The compiler takes a program and turns it into `Code`, which the interpreter can then execute to return a `Mem` representing the memory of the program. To execute a program (e.g. the given factorial program `fac10`) run `exec $ comp $ prog` where `prog` is the name of the program. It should return `[('A',3628800),('B',0)]`, of which the output is represented by the variable `A` as 3628800.
