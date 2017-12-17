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

getCoordinate: Int -> (Int, Int) -> Direction -> Int -> (Int, Int)
getCoordinate stepsTodo coordinate direction stepSize =
  if(stepsTodo < stepSize) then
    applyDelta coordinate direction stepsTodo
  else
    let
      newCoordinate = applyDelta coordinate direction stepSize
      newDirection = nextDirection direction
      newStepSize = nextStepSize stepSize newDirection
    in
      getCoordinate (stepsTodo - stepSize) newCoordinate newDirection newStepSize


solve: Int -> Int
solve input =
  let
    (x,y) = getCoordinate (input - 1) (0,0) Right 1
  in
    abs(x) + abs(y)

main =
  text (toString (solve input))
