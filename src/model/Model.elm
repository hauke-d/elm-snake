module Model exposing (..)

import Set exposing (Set)
import Json.Decode as D

--- CONSTANTS

height = 15

width = 25

minLocation = 0

maxLocation = height * width - 1

centerLocation = height // 2 * width + width // 2

clock = 20

difficultyOffset = 4

--- MODEL 

type alias Location = Int

type Direction =
    Up 
    | Right 
    | Down
    | Left

type alias Snake = List Location

type Difficulty =
    Grandma
    | Medium
    | Pro 

type GameStatus = 
    Initial 
    | Ready
    | Running 
    | Ended

type alias Model = 
    { snake: Snake
    , food: Location
    , direction: Direction
    , nextStep: Int
    , difficulty: Int
    , status: GameStatus
    }

type Msg =
    MakeNewFood
    | NewFood Location
    | Step
    | KeyDown String
    | Start
    | GameOver
    | Restart
    | ButtonClick Direction

init : () -> (Model, Cmd Msg)
init _ =
    ({ snake = []
    , food = centerLocation
    , direction = Right 
    , nextStep = 0
    , difficulty = 0
    , status = Initial
    }
    , Cmd.none
    )

isWithinBounds : Location -> Bool 
isWithinBounds location = 
    location >= minLocation && location <= maxLocation 

isAdjacent : Location -> Location -> Bool
isAdjacent from to = 
    abs (from - to) == width || abs ((remainderBy width from) - (remainderBy width to)) == 1

getFreeLocations : Snake -> Set Location
getFreeLocations snake = 
    Set.diff (Set.fromList (List.range minLocation maxLocation)) (Set.fromList snake)

isValidNewDirection : Direction -> Direction -> Bool
isValidNewDirection new current =
    let
        lr = \n -> n == Left || n == Right
        ud = \n -> n == Up || n == Down
    in
    case current of 
        Up -> lr new
        Down -> lr new
        _ -> ud new

isValidNewLocation : Location -> Model -> Bool
isValidNewLocation loc model =
    case List.head model.snake of 
        Just h ->
            (isWithinBounds loc) && not (Set.member loc (Set.fromList model.snake)) && isAdjacent loc h
        Nothing -> 
            False

step : Location -> Direction -> Location
step location direction = 
    case direction of
        Up -> location - width 
        Right -> location + 1 
        Down -> location + width 
        Left -> location - 1

snakeStep : Location -> Snake -> Snake
snakeStep head snake = 
    head :: (List.take ((List.length snake) - 1) snake)

keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


numberParser : String -> Maybe Int
numberParser key =
    case String.toInt key of 
        Just 1 -> Just 1
        Just 2 -> Just 2
        Just 3 -> Just 3
        _ -> Nothing

directionParser : String -> Maybe Direction
directionParser key =
    case key of 
        "ArrowUp" -> Just Up
        "ArrowRight" -> Just Right 
        "ArrowDown" -> Just Down 
        "ArrowLeft" -> Just Left
        _ -> Nothing