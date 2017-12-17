import Html exposing (..)
import Dict exposing (..)
import Array exposing (..)

main = text <| toString <| solve input

solve: Int -> Int
solve input =
    solve2 cycles input 0 0 1 1

-- Cursor = index relative to 0
solve2 cycles distance cursor numberAfterZero size nextNumber =
    if(cycles == 0) then
        numberAfterZero
    else
        let
            cursorAfterJump = (cursor + distance) % size
            newNumberAfterZero = 
                if(cursorAfterJump == 0) then
                    nextNumber
                else
                    numberAfterZero
        in
            solve2 (cycles - 1) distance (cursorAfterJump + 1) newNumberAfterZero (size + 1) (nextNumber + 1)

input = 304
cycles = 50000000
