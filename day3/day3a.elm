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