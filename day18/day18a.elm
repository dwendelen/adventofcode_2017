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
    = Snd String
    | Set String String
    | Add String String
    | Mul String String
    | Mod String String
    | Rcv String
    | Jgz String String
    | Error

solve: String -> Int
solve input =
    let
        instructions =
            String.split "\n" input
            |> List.map parseInstruction
            |> Array.fromList
    in
        executeInstructions instructions 0 Dict.empty
    
    
parseInstruction: String -> Instruction
parseInstruction line =
    let
        pieces = String.split " " line |> Array.fromList
        instr = Array.get 0 pieces
        arg1 = Array.get 1 pieces
        arg2 = Array.get 2 pieces
    in
        (case instr of
            Just "snd" ->
                Maybe.map Snd arg1
            Just "set" ->
                Maybe.map2 Set arg1 arg2
            Just "add" ->
                Maybe.map2 Add arg1 arg2
            Just "mul" -> 
                Maybe.map2 Mul arg1 arg2
            Just "mod" ->
                Maybe.map2 Mod arg1 arg2
            Just "rcv" ->
                Maybe.map Rcv arg1
            Just "jgz" ->
                Maybe.map2 Jgz arg1 arg2
            _ -> Nothing)
        |> Maybe.withDefault Error

type ExecutionResult
    = Jump Int
    | UpdateRegister String Int
    | Return Int
    | Skip
    | ErrorResult
    
executeInstructions: Array Instruction -> Int -> Dict String Int -> Int
executeInstructions instructions index registers =
    let
        result = Array.get index instructions
            |> Maybe.map (executeInstruction registers)
            |> Maybe.withDefault ErrorResult
    in
        case result of
            ErrorResult ->
                999999999
            Return r ->
                r
            Skip ->
                executeInstructions instructions (index + 1) registers
            Jump offset ->
                executeInstructions instructions (index + offset) registers
            UpdateRegister key val ->
                executeInstructions instructions (index + 1) (Dict.insert key val registers)
 
executeInstruction registers instruction =
    case instruction of
        Snd a ->
            UpdateRegister "snd" (getValue a registers)
        Set a b ->
            UpdateRegister a (getValue b registers)
        Add a b ->
            math (+) a b registers
        Mul a b ->
            math (*) a b registers
        Mod a b -> 
            math (%) a b registers
        Rcv a ->
            let
                v = getValue "snd" registers
            in
                if v == 0 then
                    Skip
                else
                    Return (getValue "snd" registers)
        Jgz a b ->
            if (getValue a registers) > 0 then
                Jump (getValue b registers)
            else
                Skip
        Error ->
            ErrorResult

getValue string registers =
    case (String.toInt string) of
        Ok i -> i
        Err _ -> Dict.get string registers |> Maybe.withDefault 0

math func a b registers =
    let
        newVal = func (getValue a registers) (getValue b registers)
    in
        UpdateRegister a newVal

input = """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 680
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""

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
