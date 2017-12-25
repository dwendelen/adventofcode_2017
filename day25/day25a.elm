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

type State
    = A
    | B
    | C
    | D
    | E
    | F
    
    | AA
    | BB
    
solve: Int -> Int
solve input =
    getNbOfOnes input 0 Dict.empty A

input2 = 6

input = 12667664



getNbOfOnes: Int -> Int -> Dict Int Bool -> State -> Int
getNbOfOnes cnt index memory state =
    if cnt == 0 then
        Dict.values memory
        |> List.filter identity
        |> List.length
    else
        let
            (newIndex, newMemory, newState) = run index memory state
        in
            getNbOfOnes (cnt - 1) newIndex newMemory newState

run: Int -> Dict Int Bool -> State -> (Int, Dict Int Bool, State)
run index memory state =
    let
        value = read index memory
        (newVal, dIdx, newState) = eval value state
        newMemory = write index newVal memory
        newIndex = index + dIdx
    in
        --Debug.log "state" 
        (newIndex, newMemory, newState)

eval value state =
    case state of
        A ->
            if not value then
                (True, 1, B)
            else
                (False, -1, C)
        B ->
            if not value then
                (True, -1, A)
            else
                (True, 1, D)
        C ->
            if not value then
                (False, -1, B)
            else
                (False, -1, E)
        D ->
            if not value then
                (True, 1, A)
            else
                (False, 1, B)
        E ->
            if not value then
                (True, -1, F)
            else
                (True, -1, C)
        F ->
            if not value then
                (True, 1, D)
            else
                (True, 1, A)
                
        AA ->
            if not value then
                (True, 1, BB)
            else
                (False, -1, BB)
        BB -> 
            if not value then
                (True, -1, AA)
            else
                (True, 1, AA)                    

read: Int -> Dict Int Bool -> Bool
read index memory =
    Dict.get index memory |> Maybe.withDefault False

write: Int -> Bool -> Dict Int Bool -> Dict Int Bool
write index value memory =
    Dict.insert index value memory

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

outerProduct: List a -> List (a, a)
outerProduct list =
  List.concatMap (\e ->
    List.map (\i -> (e, i)) list
  ) list
  
histogram: List comparable -> Dict comparable Int
histogram list =
    List.foldl histo_inc Dict.empty list

histo_inc:  comparable -> Dict comparable Int -> Dict comparable Int
histo_inc dir dict  =
    if Dict.member dir dict then
        Dict.update dir (Maybe.map ((+) 1))  dict
    else
        Dict.insert dir 1 dict
