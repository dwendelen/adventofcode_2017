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

import Html exposing (text)
import Dict exposing (..)

input = 277678


last list =
  List.reverse list
  |> List.head
  |> Maybe.withDefault 0
  
toIntOrZero: String -> Int
toIntOrZero char =
  case (String.toInt char) of
    Ok a ->
      a
    Err _ ->
      0
      
outerProduct: List Int -> List (Int, Int)
outerProduct list =
  List.concatMap (\e ->
    List.map (\i -> (e, i)) list
  ) list






type Direction 
  = Left 
  | Right
  | Up 
  | Down


coordinateDiv dir  =
  case dir of
    Left -> (-1, 0)
    Right -> (1, 0)
    Up -> (0, 1)
    Down -> (0, -1)

nextDirection dir =
  case dir of
    Left -> Down
    Right -> Up
    Up -> Left
    Down -> Right
    
nextStepSize currentStepSize newDirection =
  case newDirection of
    Left -> currentStepSize + 1
    Right -> currentStepSize + 1
    _ -> currentStepSize

applyDelta (x,y) direction delta =
  let
    (dx, dy) = coordinateDiv direction
  in
    (x + dx * delta, y + dy * delta)

getNeighbours (x, y) =
  [(x-1,y-1),(x-1, y), (x-1, y+1),(x,y-1),(x, y), (x, y+1),(x+1,y-1),(x+1, y), (x+1, y+1)]

getValueOrZero dict coordinate =
  Dict.get coordinate dict
  |> Maybe.withDefault 0

getNumber: Dict (Int,Int) Int  -> Int -> (Int,Int) -> Direction -> Int -> Int -> Int
getNumber dict stepsTodo coordinate direction stepSize input =
  if(stepsTodo == 0) then
    let
      newDirection = nextDirection direction
      newStepSize = nextStepSize stepSize newDirection
    in
      getNumber dict newStepSize coordinate newDirection newStepSize input
  else
    let
      newCoordinate =  applyDelta coordinate direction 1
      newValue = getNeighbours newCoordinate
        |> List.map (getValueOrZero dict)
        |> List.foldl (+) 0
      newDict =  Dict.insert newCoordinate newValue dict
    in
      if(newValue > input) then
        newValue
      else
        getNumber newDict (stepsTodo - 1) newCoordinate direction stepSize input


solve: Int -> Int
solve input =
  let
    initialDict = Dict.singleton (0,0) 1
  in
    getNumber initialDict 1 (0,0) Right 1 input

main =
  text (toString (solve input))
