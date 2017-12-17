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

solve: String -> Int
solve input =
    let
        list =
            input
            |> String.split ";"
            |> List.map toIntOrZero
            |> Array.fromList
    in
        solve2 [list] list 1

solve2 knownPositions list cycle  =
  let
    newList = redistribute list
  in
    if(List.member newList knownPositions) then
      cycle
    else
      solve2 (newList::knownPositions) newList (cycle + 1)

redistribute: Array Int -> Array Int
redistribute array =
    let
        (maxIdx, maxVal) = getMax array -1 -1 0
    in
        spill maxVal maxIdx (Array.set maxIdx 0 array)

getMax array maxIdx maxVal nextIdx =
    case Array.get nextIdx array of
        Nothing -> (maxIdx, maxVal)
        Just val ->
            if val > maxVal then
                getMax array nextIdx val (nextIdx + 1)
            else
                getMax array maxIdx maxVal (nextIdx + 1)
                
spill left previousIdx array =
    if left == 0 then
        array
    else
        let
            idx = (previousIdx + 1) % (Array.length array)
        in
            spill (left - 1) idx (increment idx array)

increment idx array =
    case Array.get idx array of
        Just v -> Array.set idx (v + 1) array
        Nothing -> array

-- input = "0;2;7;0"

input = "0;5;10;0;11;14;13;4;11;8;8;7;1;4;12;11"

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
      -1
