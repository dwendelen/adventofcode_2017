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