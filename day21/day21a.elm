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
    
type alias Grid a = List (List a)

type alias Rule =
    { input: Grid Char
    , output: Grid Char
    }

main = text <| toString <| solve input

solve: String -> Int
solve input =
    let
        init = parseToArray initial
        rules = parseRules input
    in
        applyRules 5 rules init
        |> countOn

applyRules: Int -> List Rule -> Grid Char -> Grid Char
applyRules nbOfTimes rules grid =
    if nbOfTimes == 0 then
        grid
    else
        applyRules (nbOfTimes - 1) rules (applyRulesOnGrid rules grid)

applyRulesOnGrid: List Rule -> Grid Char -> Grid Char     
applyRulesOnGrid rules grid =
    splitInSubGrids grid
    |> List.map (List.map (applyRulesOnSubgrid rules))
    |> mergeSubGrids
    |> Debug.log "Merged"

splitInSubGrids: Grid Char -> Grid (Grid Char)
splitInSubGrids grid =
    if (List.length grid % 2) == 0 then
        splitInSubGrids2 grid []
    else
        splitInSubGrids3 grid []

splitInSubGrids2: Grid Char -> Grid (Grid Char) -> Grid (Grid Char)
splitInSubGrids2 rows acc =
    case rows of
        [] -> acc
        row1::row2::tail ->
            splitInSubGrids2 tail (List.append acc [splitRow2 row1 row2 []])
        _ -> [[[['E']]]]

splitRow2: List Char -> List Char -> List (Grid Char) -> List (Grid Char)
splitRow2 row1 row2 acc =
    case (row1, row2) of
        ([], []) -> acc
        (c00::c01::tail1, c10::c11::tail2) ->
            splitRow2 tail1 tail2 (List.append acc [[[c00, c01], [c10, c11]]])
        _ -> [[['E']]]
          
splitInSubGrids3: Grid Char -> Grid (Grid Char) -> Grid (Grid Char)     
splitInSubGrids3 rows acc =
    case rows of
        [] -> acc
        row1::row2::row3::tail ->
            splitInSubGrids3 tail (List.append acc [splitRow3 row1 row2 row3 []])
        _ -> [[[['E']]]]

splitRow3: List Char -> List Char -> List Char -> List (Grid Char) -> List (Grid Char)
splitRow3 row1 row2 row3 acc =
    case (row1, row2, row3) of
        ([], [], []) -> acc
        (c00::c01::c02::tail1, c10::c11::c12::tail2, c20::c21::c22::tail3) ->
            splitRow3 tail1 tail2 tail3
                (List.append acc [[[c00, c01, c02], [c10, c11, c12], [c20, c21, c22]]])
        _ -> [[['E']]]

mergeSubGrids: Grid (Grid Char) -> Grid Char
mergeSubGrids subgrids =
    subgrids
    |> List.concatMap (mergeRow [])

mergeRow: Grid Char -> List (Grid Char) -> Grid Char
mergeRow acc row  =
    case row of
        [] -> acc
        subgrid::tail ->
            mergeRow (mergeGrids acc subgrid) tail

mergeGrids: Grid Char -> Grid Char -> Grid Char
mergeGrids grid1 grid2 =
    case (grid1, grid2) of
        ([], gridd) -> gridd
        ([grid1_0, grid1_1, grid1_2], [grid2_0, grid2_1, grid2_2]) ->
            [ List.append grid1_0 grid2_0
            , List.append grid1_1 grid2_1
            , List.append grid1_2 grid2_2
            ]
        ([grid1_0, grid1_1, grid1_2, grid1_3], [grid2_0, grid2_1, grid2_2, grid2_3]) ->
            [ List.append grid1_0 grid2_0
            , List.append grid1_1 grid2_1
            , List.append grid1_2 grid2_2
            , List.append grid1_3 grid2_3
            ]
        _ -> [['E']]
        
applyRulesOnSubgrid: (List Rule) -> (Grid Char) -> (Grid Char)
applyRulesOnSubgrid rules subgrid =
    rules
    |> List.filterMap (tryApplyASingleRule subgrid)
    |> List.head
    |> Maybe.withDefault (parseToArray "E")
    
tryApplyASingleRule: Grid Char -> Rule -> Maybe (Grid Char)
tryApplyASingleRule subgrid rule =
    let
        flippedAndRotated = rotateAndFlip subgrid
        isMatch = List.member rule.input flippedAndRotated
    in
        if isMatch then Just rule.output
        else Nothing

rotateAndFlip subgrid =
    let
        flp1 = flip1 subgrid
        flp2 = flip2 subgrid
        
        rot1 = rotateOnce subgrid
        rot2 = rotateOnce rot1
        rot3 = rotateOnce rot2
        
        flp1rot1 = flip1 rot1
        flp2rot1 = flip2 rot1
        
        flp1rot2 = flip1 rot2
        flp2rot2 = flip2 rot2
        
        flp1rot3 = flip1 rot3
        flp2rot3 = flip2 rot3
        
        rot1flp1 = rotateOnce flp1
        rot2flp1 = rotateOnce rot1flp1
        rot3flp1 = rotateOnce rot2flp1
        
        rot1flp2 = rotateOnce flp2
        rot2flp2 = rotateOnce rot1flp2
        rot3flp2 = rotateOnce rot2flp2
    in
        [subgrid, flp1, flp2, rot1, rot2, rot3, 
            flp1rot1, flp2rot1, flp1rot2, flp2rot2, flp1rot3, flp2rot3,
            rot1flp1, rot2flp1, rot3flp1, rot1flp2, rot2flp2, rot3flp2]

flip1 subgrid =
    case subgrid of
        [r0, r1] -> [r1, r0]
        [r0, r1, r2] -> [r2, r1, r0]
        _ -> [['E']]
            
flip2 subgrid =
    case subgrid of
        [[c00, c01], [c10, c11]] ->
            [[c01, c00], [c11, c10]]
        [[c00, c01, c02], [c10, c11, c12], [c20, c21, c22]] ->
            [[c02, c01, c00], [c12, c11, c10], [c22, c21, c20]]
        _ ->
            [['E']]

rotateOnce subgrid =
    case subgrid of
        [[c00, c01], [c10, c11]] ->
            [[c10, c00], [c11, c01]]
        [[c00, c01, c02], [c10, c11, c12], [c20, c21, c22]] ->
            [[c20, c10, c00], [c21, c11, c01], [c22, c12, c02]]
        _ ->
            [['E']]

countOn: Grid Char -> Int
countOn subgrids =
    subgrids
    |> List.concatMap identity
    |> List.filter (\c -> c == '#')
    |> List.length

parseRules rules =
    String.split "\n" input
    |> List.map parseRule

parseRule: String -> Rule
parseRule rule =
    let
        pieces = String.split " => " rule |> Array.fromList
        input = Array.get 0 pieces |> Maybe.map parseToArray
        output = Array.get 1 pieces |> Maybe.map parseToArray
     in
        Maybe.map2 (\i o -> {input = i, output = o}) input output
        |> Maybe.withDefault {input = [['E']], output = [['E']]}
    
parseToArray: String -> List (List Char)
parseToArray line =
    String.split "/" line
        |> List.map String.toList
        


initial = ".#./..#/###"

input2 = """../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"""

input = """../.. => ..#/.#./...
#./.. => .../#../.##
##/.. => .##/###/##.
.#/#. => #.#/..#/#.#
##/#. => .../.##/...
##/## => ##./..#/..#
.../.../... => ##../..../##../.###
#../.../... => ...#/.#.#/.#../.#.#
.#./.../... => #.#./...#/#.#./.##.
##./.../... => ..#./#.##/#.../.###
#.#/.../... => ##../##.#/..#./#.##
###/.../... => ..../.#.#/.###/#..#
.#./#../... => #..#/#.../.##./....
##./#../... => #.##/..##/####/.###
..#/#../... => ..#./#.##/####/####
#.#/#../... => .##./#.##/#.#./##.#
.##/#../... => #.##/####/.###/...#
###/#../... => ..../#.#./##.#/..##
.../.#./... => .###/.##./##../.##.
#../.#./... => ..../#.##/...#/#.#.
.#./.#./... => ...#/####/.##./#...
##./.#./... => .###/#.##/###./....
#.#/.#./... => #.##/###./..../..#.
###/.#./... => .#../#.#./#.##/#.##
.#./##./... => .###/##../..##/#..#
##./##./... => ..#./#.#./.#.#/##.#
..#/##./... => .#../####/...#/..##
#.#/##./... => ..../##.#/.##./....
.##/##./... => .#.#/.#.#/.##./####
###/##./... => ##.#/..../..../....
.../#.#/... => ..##/##../##.#/###.
#../#.#/... => ####/#.##/#.../###.
.#./#.#/... => ..../#..#/..##/.#..
##./#.#/... => #.../..##/##../..#.
#.#/#.#/... => ...#/#.#./#.#./#...
###/#.#/... => ###./###./##.#/###.
.../###/... => ..#./###./##.#/####
#../###/... => ##.#/..#./##../..##
.#./###/... => #.../#.##/##../....
##./###/... => ..##/.#.#/#..#/#.##
#.#/###/... => #.##/..#./.#../..##
###/###/... => ..#./#..#/####/.##.
..#/.../#.. => ##.#/#.##/...#/###.
#.#/.../#.. => #..#/..#./##../###.
.##/.../#.. => ..#./.#../###./#.#.
###/.../#.. => ...#/...#/.#.#/.##.
.##/#../#.. => ##../#.#./#..#/##..
###/#../#.. => ##../.#.#/##../#..#
..#/.#./#.. => ##.#/##.#/...#/.#..
#.#/.#./#.. => .###/.#.#/###./....
.##/.#./#.. => #..#/###./####/..#.
###/.#./#.. => ..#./.###/.###/...#
.##/##./#.. => #.##/..##/...#/.###
###/##./#.. => ####/##.#/#.##/#..#
#../..#/#.. => ..../.##./#.##/#...
.#./..#/#.. => #..#/##../...#/#...
##./..#/#.. => ..#./.###/..##/.#.#
#.#/..#/#.. => .##./..##/..#./#..#
.##/..#/#.. => ####/.#.#/#.../.#.#
###/..#/#.. => ..../..##/#.##/###.
#../#.#/#.. => #.##/.#.#/.#../.##.
.#./#.#/#.. => ..##/###./.###/###.
##./#.#/#.. => ##.#/##.#/#.#./##..
..#/#.#/#.. => ###./###./.#.#/.#..
#.#/#.#/#.. => ##../..#./##../....
.##/#.#/#.. => .###/#.#./##.#/##..
###/#.#/#.. => ##.#/#.#./.#.#/#...
#../.##/#.. => .#.#/...#/.#.#/..#.
.#./.##/#.. => ###./##../##.#/....
##./.##/#.. => ..##/###./#.#./#.#.
#.#/.##/#.. => ##.#/..##/#..#/####
.##/.##/#.. => ..../####/..#./##..
###/.##/#.. => .###/#..#/..../.#..
#../###/#.. => #..#/.#../.#.#/#...
.#./###/#.. => .#../..../.##./.###
##./###/#.. => ##.#/.#../.#.#/#..#
..#/###/#.. => #.##/##../..##/#...
#.#/###/#.. => ####/..##/.#../##.#
.##/###/#.. => .###/#..#/.###/#.##
###/###/#.. => ..##/.##./##../#..#
.#./#.#/.#. => ..##/.##./.##./.###
##./#.#/.#. => ..##/...#/.##./####
#.#/#.#/.#. => .###/.###/#.#./.#..
###/#.#/.#. => ##.#/###./##.#/####
.#./###/.#. => ...#/..#./.#.#/.#..
##./###/.#. => ###./##.#/#.../#.#.
#.#/###/.#. => .##./#.#./...#/..#.
###/###/.#. => .#.#/.#../..##/####
#.#/..#/##. => .##./...#/#..#/.###
###/..#/##. => #.##/.#.#/...#/..##
.##/#.#/##. => ###./.###/...#/....
###/#.#/##. => .##./.##./#.#./#...
#.#/.##/##. => #.#./.##./.#.#/.###
###/.##/##. => ..../####/.#.#/#.##
.##/###/##. => .##./.###/###./.#..
###/###/##. => #.../###./.##./##.#
#.#/.../#.# => #.#./..../#.##/###.
###/.../#.# => .#../.#.#/#.../.###
###/#../#.# => ###./#..#/####/##..
#.#/.#./#.# => ###./##.#/..../.#..
###/.#./#.# => ####/.#.#/.#../..##
###/##./#.# => #.#./####/..##/#...
#.#/#.#/#.# => #.#./#.#./#.../#.##
###/#.#/#.# => #.##/.#../..#./.##.
#.#/###/#.# => .###/..##/####/#..#
###/###/#.# => #.../..#./..#./#.##
###/#.#/### => .#.#/.###/#.##/..##
###/###/### => #.#./...#/.#../.#.#"""

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
  
map9 f e1 e2 e3 e4 e5 e6 e7 e8 e9 =
    let
        r0 = Maybe.map3 (\i j k -> (i, j, k)) e1 e2 e3
        r1 = Maybe.map3 (\i j k -> (i, j, k)) e4 e5 e6
        r2 = Maybe.map3 (\i j k -> (i, j, k)) e7 e8 e9
     in
        Maybe.map3 (map9_2 f) r0 r1 r2
        
map9_2 f (e1, e2, e3) (e4, e5, e6) (e7, e8, e9) =
    f e1 e2 e3 e4 e5 e6 e7 e8 e9
