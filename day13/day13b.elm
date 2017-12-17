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

--input = "0: 3;1: 2;4: 4;6: 4"
input = "0: 4;1: 2;2: 3;4: 4;6: 8;8: 5;10: 8;12: 6;14: 6;16: 8;18: 6;20: 6;22: 12;24: 12;26: 10;28: 8;30: 12;32: 8;34: 12;36: 9;38: 12;40: 8;42: 12;44: 17;46: 14;48: 12;50: 10;52: 20;54: 12;56: 14;58: 14;60: 14;62: 12;64: 14;66: 14;68: 14;70: 14;72: 12;74: 14;76: 14;80: 14;84: 18;88: 14"

toIntOrZero: String -> Int
toIntOrZero char =
  case (String.toInt char) of
    Ok a ->
      a
    Err _ ->
      0

calcDamage: Int -> String -> Int
calcDamage delay line  =
  case String.split ": " line |> List.map toIntOrZero  of
    time::range::[] -> 
      let
        patrolTime = 2 * (range - 1)
      in
        if((delay + time) % patrolTime == 0) then
          1
        else
          0
    _ -> 0

        
tryDelay: String -> Int -> Int
tryDelay input delay =
  let
    damage = String.split ";" input
    |> List.map (calcDamage delay)
    |> List.foldl (+) 0
  in
    if(damage == 0) then
      delay
    else
      tryDelay input (delay + 1)

solve: String -> Int
solve input =
  tryDelay input 0

main = text <| toString <| solve input
