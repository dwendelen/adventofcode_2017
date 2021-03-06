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

type NodeState
    = Clean
    | Weakened
    | Infected
    | Flagged

solve: String -> Int
solve input =
    let
        (start, infected) = parse input
    in
        doBursts 10000000 0 start (0, 1) infected

doBursts nbOfBursts acc (px, py) (dx, dy) infected =
    if nbOfBursts == 0 then
        acc
    else
        let
            oldState = Dict.get (px, py) infected |> Maybe.withDefault Clean
            (ndx, ndy) =
                case oldState of
                    Clean -> (-dy, dx) -- Left
                    Weakened -> (dx, dy) -- Nothing
                    Infected -> (dy, -dx) -- Right
                    Flagged -> (-dx, -dy) -- Reverse
            newInfected =
                Dict.insert (px, py) (nextState oldState) infected
                
            newAcc = if oldState == Weakened then (acc + 1) else acc
            newPos = (px + ndx, py + ndy)
        in
            doBursts (nbOfBursts - 1) newAcc newPos (ndx, ndy) newInfected

nextState state =
    case state of
        Clean -> Weakened
        Weakened -> Infected
        Infected -> Flagged
        Flagged -> Clean

parse input =
    let
        chars =
            String.split "\n" input
            |> List.indexedMap (\l row ->
                String.toList row
                |> List.indexedMap (\x char ->
                    ((x, -l), char)
                   )
               )
        infected = 
            chars
            |> List.concatMap identity
            |> List.filter (\(_,c) -> c == '#')
            |> List.map (\(p, _) -> (p, Infected))
            |> Dict.fromList
            
        startY = -(((List.length chars) - 1) // 2)
        startX =
            List.head chars
            |> Maybe.map (\l -> ((List.length l) - 1) // 2)
            |> Maybe.withDefault 0
    in
        ((startX, startY), infected)
    
input2 = """..#
#..
..."""

input = """..######.###...######...#
.##..##.#....#..##.#....#
.##.#....###..##.###.#.#.
#.#.###.#####.###.##.##.#
.###.#.#.###.####..##.###
..####.##..#.#.#####...##
....##.###..#.#..#...####
.#.##.##.#..##...##.###..
.######..#..#.#####....##
###.##.###.########...###
.#.#.#..#.##.#..###...#..
.#.##.#.####.#.#.....###.
##..###.###..##...#.##.##
##.#.##..#...##...#...###
##..#..###.#..##.#.#.#.#.
.##.#####..##....#.#.#..#
..#.######.##...#..#.##..
#.##...#.#....###.#.##.#.
.#..#.#.#..#.####..#.####
.##...##....##..#.#.###..
..##.#.#.##..##.#.#....#.
###.###.######.#.########
..#.####.#.#.##..####...#
#.##..#.#.####...#..#..##
###.###.#..##..#.###....#"""

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
