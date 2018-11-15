import Browser
import Browser.Events exposing (onKeyDown)
import Time
import Array exposing (Array)
import Set exposing (Set)
import Random
import Json.Decode as D
import Html exposing (Html, pre, div, text, button, table, tr, td)
import Html.Events exposing (onClick)
import Html.Attributes exposing (tabindex, style)

height = 15

width = 25

minLocation = 0

centerLocation = height // 2 * width + width // 2

maxLocation = height * width - 1

clock = 20
difficultyOffset = 4


type alias Location = Int

type alias Snake = List Location

type Direction =
    Up 
    | Right 
    | Down
    | Left

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

main = 
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

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


initSnake : Snake
initSnake = List.reverse (List.range (centerLocation - 1) (centerLocation + 1))

initFood : Location
initFood = centerLocation + 5

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ keyDownSubscription, timeSubscription model ]

keyDownSubscription : Sub Msg 
keyDownSubscription =
    onKeyDown (D.map (KeyDown) keyDecoder)

timeSubscription : Model -> Sub Msg 
timeSubscription model = 
    if model.status == Running then 
        Time.every clock (\_ -> Step)
    else
        Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        MakeNewFood ->
            ( model
            , Random.generate NewFood (Random.int 0 (Set.size (getFreeLocations model.snake)))
            )
        NewFood food -> 
            ( { model | food = Maybe.withDefault 0 (pick food (getFreeLocations model.snake)) }
            , Cmd.none
            )
        Start ->
            ( { model | status = Running }
            , Cmd.none
            )
        Step -> 
            if model.nextStep == 0 then
                updateSnake model
            else
                ({ model | nextStep = model.nextStep - 1}
                , Cmd.none
                )
        KeyDown key ->
            updateKeyDown key model
        ButtonClick d ->
            updateDirection (Just d) model
        GameOver ->
            ( { model | status = Ended }
            , Cmd.none
            )
        Restart ->
            init ()
        


view : Model -> Html Msg 
view model = 
    div [tabindex 0, style "outline" "none", style "text-align" "center"] 
        [ pre [style "font-family" "monospace"] [ text viewHeader ]
        , viewBody model
        , viewFooter model
        ]

viewDifficultyKeys : Html Msg
viewDifficultyKeys = 
    div [] 
        [ div 
            [ style "position" "relative"
            , style "width" "250px"
            , style "display" "inline-block"
            , style "margin-bottom" "50px"
            ]
            [ viewKey 0 0 "1" (KeyDown "1")
            , viewKey 0 2 "2" (KeyDown "2")
            , viewKey 0 4 "3" (KeyDown "3")
            ]
        , pre []
            [ text "GRANDMA      MEDIUM       BADASS"

            ]
        ]

viewArrowKeys : Html Msg 
viewArrowKeys =
    div [ style "position" "relative"
        , style "width" "150px"
        , style "display" "inline-block"
        , style "margin-bottom" "150px"
        ]
        [ viewKey 0 1 "^" (KeyDown "ArrowUp")
        , viewKey 1 0 "<" (KeyDown "ArrowLeft")
        , viewKey 1 2 ">" (KeyDown "ArrowRight")
        , viewKey 2 1 "v" (KeyDown "ArrowDown")
        ]

viewKey : Int -> Int -> String -> Msg -> Html Msg 
viewKey top left content msg =
    button 
        [ style "position" "absolute"
        , style "width" "50px"
        , style "height" "50px"
        , style "font-size" "30px"
        , style "background" "#fff"
        , style "border" "1px solid #555"
        , style "border-radius" "25px"
        , style "outline" "none"
        , style "top" (String.fromInt (top * 50) ++ "px")
        , style "left" (String.fromInt (left * 50) ++ "px")
        , onClick msg
        ]
        [ text content ]


viewHeader : String 
viewHeader =
    "     __  _       _  _  ____ \n" ++
    " ___|  \\| |_____| |/ /| ___|\n" ++
    "| __|     |  _  |   / | __| \n" ++
    "|__ |_|\\__| |_| |   \\ |____|\n" ++
    "|___|     |  _  |_|\\_\\      \n" ++
    "          |_| |_|           \n"

viewFooter : Model -> Html Msg
viewFooter model = 
    case model.status of 
        Initial -> 
            pre [] [text "CHOOSE DIFFICULTY"]
        Ready ->
            div []
                [ viewArrowKeys
                , pre [] [text "USE KEYBOARD OR ARROWS TO CONTROL\nPRESS ANY KEY TO START"]
                ]
        Running ->
            div []
                [ viewArrowKeys
                , pre [] [text "USE KEYBOARD OR ARROWS TO CONTROL"]
                ]
        Ended -> 
            div [ style "position" "relative"
                , style "width" "200px"
                , style "display" "inline-block"
                ]
                [ pre []
                    [ text "GAME OVER!\n\n"
                    , text ("DIFFICULTY: " ++ String.fromInt model.difficulty ++ "\n") 
                    , text ("POINTS:     " ++ String.fromInt (List.length model.snake) ++ "\n\n")
                    , text "PRESS ANY KEY TO RESTART"
                    ]
                , div 
                    [ style "position" "relative" ]
                    [ button 
                        [ style "position" "absolute"
                        , style "width" "100px"
                        , style "height" "50px"
                        , style "font-size" "20px"
                        , style "background" "#fff"
                        , style "border" "1px solid #555"
                        , style "border-radius" "25px"
                        , style "outline" "none"
                        , style "top" "0"
                        , style "left" "50px"
                        , onClick (KeyDown "x")
                        ]
                        [ text "ANY" ]
                    ]
                ]
            
viewBody : Model -> Html Msg 
viewBody model =
    case model.status of 
        Initial ->
            viewDifficultyKeys
        _ -> 
            viewBoard model

viewBoard : Model -> Html Msg
viewBoard model = 
    pre 
        [ style "font-family" "monospace"
        , style "line-height" "1"
        ] 
        [ text (String.join "\n" (viewRows (Set.fromList model.snake) model.food))]

viewRows : Set Location -> Location -> List String
viewRows snake food = 
    List.map (\row -> viewColumns snake food row) (List.range -1 height)


viewColumns : Set Location -> Location -> Int -> String
viewColumns snake food row =
    let
        offset = row * width
        locations = List.range offset (offset + width - 1)
    in
    "|" ++ (String.concat (List.map (printLocation snake food) locations)) ++ "|"


printLocation : Set Location -> Location -> Location -> String 
printLocation snake food loc = 
    if (Set.member loc snake) then 
        "O" 
    else if loc == food  then 
        "X" 
    else if (isWithinBounds loc) then 
        " "
    else
        "—"


pick : Int -> Set a -> Maybe a
pick idx set = 
    Array.get idx (Array.fromList (Set.toList set))

updateKeyDown : String -> Model -> (Model, Cmd Msg)
updateKeyDown key model =
    case model.status of
        Initial ->
            updateDifficulty (numberParser key) model
        Ready ->
            update Start model
        Running ->
            updateDirection (keyParser key) model
        Ended -> 
            case keyParser key of
                Nothing ->
                    update Restart model
                Just _ ->
                    ( model, Cmd.none )

updateDifficulty : Maybe Int -> Model -> (Model, Cmd Msg)
updateDifficulty d model =
    case d of 
        Just n -> 
            ( { model | difficulty = n, status = Ready, snake = initSnake, food = initFood }
            , Cmd.none
            )
        Nothing -> 
            ( model, Cmd.none )

updateDirection : Maybe Direction -> Model -> (Model, Cmd Msg)
updateDirection direction model =
    case direction of 
        Just d ->
            if isValidNewDirection d model.direction then
                update Step { model | direction = d, nextStep = 0 }
            else
                ( model, Cmd.none )
        Nothing -> 
            ( model, Cmd.none )


updateSnake : Model -> (Model, Cmd Msg)
updateSnake model = 
    let
        head = Maybe.map (\l -> step l model.direction) (List.head model.snake)
        nextModel = { model | nextStep = (3 - model.difficulty) * 3 + difficultyOffset }
    in
    case head of
        Just h ->
            if h == model.food then 
                update MakeNewFood { nextModel | snake = h :: model.snake }
            else if isValidNewLocation h model then
                ({ nextModel | snake = snakeStep h model.snake }
                , Cmd.none
                )
            else
                update GameOver nextModel
        Nothing -> 
            ( nextModel, Cmd.none )

snakeStep : Location -> Snake -> Snake
snakeStep head snake = 
    head :: (List.take ((List.length snake) - 1) snake)

isWithinBounds : Location -> Bool 
isWithinBounds location = 
    location >= minLocation && location <= maxLocation 

isAdjacent : Location -> Location -> Bool
isAdjacent from to = 
    abs (from - to) == width || abs ((remainderBy width from) - (remainderBy width to)) == 1

isValidNewLocation : Location -> Model -> Bool
isValidNewLocation loc model =
    case List.head model.snake of 
        Just h ->
            (isWithinBounds loc) && not (Set.member loc (Set.fromList model.snake)) && isAdjacent loc h
        Nothing -> 
            False


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

step : Location -> Direction -> Location
step location direction = 
    case direction of
        Up -> location - width 
        Right -> location + 1 
        Down -> location + width 
        Left -> location - 1


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

keyParser : String -> Maybe Direction
keyParser key =
    case key of 
        "ArrowUp" -> Just Up
        "ArrowRight" -> Just Right 
        "ArrowDown" -> Just Down 
        "ArrowLeft" -> Just Left
        _ -> Nothing