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