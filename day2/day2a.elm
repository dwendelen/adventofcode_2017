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

input = "409,194,207,470,178,454,235,333,511,103,474,293,525,372,408,428;4321,2786,6683,3921,265,262,6206,2207,5712,214,6750,2742,777,5297,3764,167;3536,2675,1298,1069,175,145,706,2614,4067,4377,146,134,1930,3850,213,4151;2169,1050,3705,2424,614,3253,222,3287,3340,2637,61,216,2894,247,3905,214;99,797,80,683,789,92,736,318,103,153,749,631,626,367,110,805;2922,1764,178,3420,3246,3456,73,2668,3518,1524,273,2237,228,1826,182,2312;2304,2058,286,2258,1607,2492,2479,164,171,663,62,144,1195,116,2172,1839;114,170,82,50,158,111,165,164,106,70,178,87,182,101,86,168;121,110,51,122,92,146,13,53,34,112,44,160,56,93,82,98;4682,642,397,5208,136,4766,180,1673,1263,4757,4680,141,4430,1098,188,1451;158,712,1382,170,550,913,191,163,459,1197,1488,1337,900,1182,1018,337;4232,236,3835,3847,3881,4180,4204,4030,220,1268,251,4739,246,3798,1885,3244;169,1928,3305,167,194,3080,2164,192,3073,1848,426,2270,3572,3456,217,3269;140,1005,2063,3048,3742,3361,117,93,2695,1529,120,3480,3061,150,3383,190;489,732,57,75,61,797,266,593,324,475,733,737,113,68,267,141;3858,202,1141,3458,2507,239,199,4400,3713,3980,4170,227,3968,1688,4352,4168"

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
      
      
      
      
solveLine: String -> Int
solveLine line =
  let 
    numbers = 
      String.split "," line
      |> List.map toIntOrZero
  in
     (Maybe.withDefault 0 (List.maximum numbers)) - (Maybe.withDefault 0 (List.minimum numbers))



solve: String -> Int
solve input =
  String.split ";" input
  |> List.map solveLine
  |> List.foldl (+) 0


main =
  text (toString (solve input))
  
