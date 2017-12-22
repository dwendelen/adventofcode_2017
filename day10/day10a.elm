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

size = 256

-- solve: List Int -> String
solve input =
    let
        initial = List.range 0 (size - 1)
        (list, index) = execute input 0 0 initial
        indexFirstElement = (size - index) % size
        indexSecondElement = (size - index + 1) % size
        
        array = Array.fromList list
        e1 = Array.get indexFirstElement array
        e2 = Array.get indexSecondElement array
    in
        Maybe.map2 (*) e1 e2

execute commands skipSize index list =
    case commands of
        [] -> (list, index)
        cmd::tail ->
            let
                toSwap = List.take cmd list
                rest = List.drop cmd list
                newList = List.append (List.reverse toSwap) rest
                
                offset = (cmd + skipSize) % size
                offsetList1 = List.drop offset newList
                offsetList2 = List.take offset newList
                offsetList = List.append offsetList1 offsetList2
                newIndex = (index + offset) % size
            in
                execute tail (skipSize + 1) newIndex offsetList
         
input2 = [3,4,1,5]

input = [212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164]

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
