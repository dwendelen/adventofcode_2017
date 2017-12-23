-- Copyright (c) 2017 Daan Wendelen
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- * Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Html exposing (..)
import Dict exposing (..)
import Array exposing (..)
import Debug exposing (..)

main = text <| toString <| solve input

type Instruction
    = Set String String
    | Sub String String
    | Mul String String
    | Jnz String String
    | Error

type ProgramState = Running | Terminated

type alias Program =
    { registers: Dict String Int
    , index: Int
    , state: ProgramState
    , nbOfMuls: Int
    }

solve: String -> Int
solve input =
    let
        instructions =
            String.split "\n" input
            |> List.map parseInstruction
            |> Array.fromList
        program =
            { registers = Dict.insert "p" 1 Dict.empty
            , index = 0
            , state = Running
            , nbOfMuls = 0
            }
    in
        executeInstructions instructions program
    
    
parseInstruction: String -> Instruction
parseInstruction line =
    let
        pieces = String.split " " line |> Array.fromList
        instr = Array.get 0 pieces
        arg1 = Array.get 1 pieces
        arg2 = Array.get 2 pieces
    in
        (case instr of
            Just "set" ->
                Maybe.map2 Set arg1 arg2
            Just "sub" ->
                Maybe.map2 Sub arg1 arg2
            Just "mul" -> 
                Maybe.map2 Mul arg1 arg2
            Just "jnz" ->
                Maybe.map2 Jnz arg1 arg2
            _ -> Nothing)
        |> Maybe.withDefault Error
    
executeInstructions: Array Instruction -> Program -> Int
executeInstructions instructions program =
    let
        newProgram = executeProgram instructions program
    in
        if newProgram.state == Running then
            executeInstructions instructions newProgram
        else
            newProgram.nbOfMuls

type ExecutionResult
    = Jump Int
    | UpdateRegister String Int
    | Skip
    | Terminate
     
executeProgram instructions program =
    let
        (newMuls, result) = Array.get program.index instructions
            |> Maybe.map (executeInstruction program.registers program.nbOfMuls)
            |> Maybe.withDefault (program.nbOfMuls, Terminate)
    in
        case result of
            Terminate ->
                {program | state = Terminated, nbOfMuls = newMuls}
            Skip ->
                {program | index = program.index + 1, nbOfMuls = newMuls}
            Jump offset ->
                {program | index = program.index + offset, nbOfMuls = newMuls}
            UpdateRegister key val ->
                let
                    newProg = 
                    {program 
                        | index = program.index + 1
                        , registers = Dict.insert key val program.registers
                        , nbOfMuls = newMuls}
                in
                    newProg
            
 
executeInstruction registers nbOfMuls instruction =
    case instruction of
        Set a b ->
            (nbOfMuls, UpdateRegister a (getValue b registers))
        Sub a b ->
            (nbOfMuls, math (-) a b registers)
        Mul a b ->
            (nbOfMuls + 1, math (*) a b registers)
        Jnz a b ->
            if (getValue a registers) /= 0 then
                (nbOfMuls, Jump (getValue b registers))
            else
                (nbOfMuls, Skip)
        Error ->
            (nbOfMuls, Terminate)

getValue string registers =
    case (String.toInt string) of
        Ok i -> i
        Err _ -> Dict.get string registers |> Maybe.withDefault 0

math func a b registers =
    let
        newVal = func (getValue a registers) (getValue b registers)
    in
        UpdateRegister a newVal

input = """set b 84
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23"""

zip: List a -> List b -> List (a, b)
zip list1 list2 =
    zip2 list1 list2 []

zip2: List a -> List b -> List(a, b) -> List(a, b)
zip2 list1 list2 acc =
    case list1 of
        [] -> acc
        head1::tail1 ->
            case list2 of
                [] -> acc
                head2::tail2 ->
                    zip2 tail1 tail2 (List.append acc [(head1, head2)])

toIntOrZero: String -> Int
toIntOrZero char =
  case (String.toInt char) of
    Ok a ->
      a
    Err _ ->
      0
