import Html exposing (..)
import Dict exposing (..)
import Array exposing (..)

main = text <| toString <| solve input

solve: Int -> Int
solve input =
    solve2 2017 input 1 [0]

solve2 cycles distance nextNumber list =
    case cycles of
        0 ->
            List.tail list |> Maybe.withDefault [] 
            |> List.head |> Maybe.withDefault -1
        _ ->
            let
                newList = jump distance list
            in
                case newList of
                    [] -> -1
                    cursor::rest ->
                        solve2 (cycles - 1) distance (nextNumber + 1) (List.append (nextNumber::rest) [cursor])
                        
jump distance list =
    case distance of
        0 -> list
        _ ->
            case list of
                [] -> []
                cursor::rest ->
                    jump (distance - 1) (List.append rest [cursor])
            

input = 304
