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
import Set exposing (..)

main = text <| toString <| solve input

solve: String -> (Int, Int)
solve input =
    lengthAndStrength (parse input) 0 0 0

lengthAndStrength: Set (Int,Int) -> Int -> Int -> Int -> (Int, Int)
lengthAndStrength pieces portt length acc =
    pieces
    |> Set.filter (\(a,b) -> a == portt || b == portt)
    |> Set.map (\(a,b) -> 
            let
                newPieces = Set.remove (a,b) pieces
            in
                if a == portt then
                    lengthAndStrength newPieces b (length + 1) (acc + a + b)
                else
                    lengthAndStrength newPieces a (length + 1) (acc + a + b)
        )
    |> Set.foldl max (length, acc)

parse input =
    String.split "\n" input
    |> List.map parseLine
    |> Set.fromList
        
parseLine line =
    case String.split "/" line of
        a::b::[] -> (toIntOrZero a, toIntOrZero b)
        _ -> (-1,-1)

input2 = """0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"""

input = """24/14
30/24
29/44
47/37
6/14
20/37
14/45
5/5
26/44
2/31
19/40
47/11
0/45
36/31
3/32
30/35
32/41
39/30
46/50
33/33
0/39
44/30
49/4
41/50
50/36
5/31
49/41
20/24
38/23
4/30
40/44
44/5
0/43
38/20
20/16
34/38
5/37
40/24
22/17
17/3
9/11
41/35
42/7
22/48
47/45
6/28
23/40
15/15
29/12
45/11
21/31
27/8
18/44
2/17
46/17
29/29
45/50"""

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
