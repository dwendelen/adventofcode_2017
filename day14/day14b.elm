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
import Hex exposing (toString)
import Char exposing (toCode)
import Bitwise exposing (xor)
import Set exposing (..)

main = text <| Basics.toString <| solve input

solve: String -> Int
solve input =
    let
        allLocations =
            List.range 0 127
            |> List.map (\i -> input ++ "-" ++ (Basics.toString i))
            |> List.map knot
            
            |> List.indexedMap (\index line ->
                String.toList line
                |> List.indexedMap (\hIndex char ->
                    getHValues char
                    |> List.map (\hv -> (index, hIndex * 4 + hv))
                )
                |> List.concatMap identity
            )
            |> List.concatMap identity
            |> Set.fromList
    in
        countNbOfGroups allLocations 0
    
countNbOfGroups set acc =
    let
        any = selectAny set
    in
        case any of
            Nothing -> acc
            Just a ->
                countNbOfGroups (removeGroup a set) (acc + 1)
        
removeGroup (x, y) set =
    if Set.member (x, y) set then
        Set.remove (x, y) set
        |> removeGroup (x-1, y)
        |> removeGroup (x+1, y)
        |> removeGroup (x, y-1)
        |> removeGroup (x, y+1)
    else
        set

selectAny set =
    Set.toList set 
    |> List.head
    
getHValues: Char -> List Int
getHValues char =
    case char of
        '0' -> []
        '1' -> [3]
        '2' -> [2]
        '3' -> [2,3]
        '4' -> [1]
        '5' -> [1,3]
        '6' -> [1,2]
        '7' -> [1,2,3]
        '8' -> [0]
        '9' -> [0,3]
        'a' -> [0,2]
        'b' -> [0,2,3]
        'c' -> [0,1]
        'd' -> [0,1,3]
        'e' -> [0,1,2]
        'f' -> [0,1,2,3]
        _ -> [999999999999]

input = "ugkiagan"

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
               
knot: String -> String
knot input =
    let
        initial = List.range 0 255
        binary = 
            String.toList input
            |> List.map Char.toCode  
        appended = List.append binary [17, 31, 73, 47, 23]
        repeated =
            List.repeat 64 appended
            |> List.concatMap identity
        list = knot_oneIteration repeated initial
    in
        list
        |> List.map Hex.toString
        |> List.map (String.padLeft 2 '0')
        |> String.join ""


knot_oneIteration commands list =
    let
        res = knot_execute commands 0 0 list
    in
        knot_compress res [] 

knot_compress list acc =
    case list of
        [] -> acc
        _ ->
            let
                tail = List.drop 16 list
                newNumber = 
                    List.take 16 list
                    |> List.foldl (Bitwise.xor) 0
             in
                knot_compress tail (List.append acc [newNumber])

knot_execute commands skipSize index list =
    case commands of
        [] ->
            let
                afterIndex = List.take (256 - index) list
                beforeIndex = List.drop (256 - index) list
            in
                List.append beforeIndex afterIndex
        cmd::tail ->
            let
                toSwap = List.take cmd list
                rest = List.drop cmd list
                newList = List.append (List.reverse toSwap) rest
                
                offset = (cmd + skipSize) % 256
                offsetList1 = List.drop offset newList
                offsetList2 = List.take offset newList
                offsetList = List.append offsetList1 offsetList2
                newIndex = (index + offset) % 256
            in
                knot_execute tail (skipSize + 1) newIndex offsetList        
