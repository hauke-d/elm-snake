module Update exposing (update)

import Model exposing (..)
import Random
import Set exposing (Set)
import Array exposing (Array)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        MakeNewFood ->
            ( model
            , Random.generate NewFood (Random.int 0 (Set.size (Model.getFreeLocations model.snake)))
            )
        NewFood food -> 
            ( { model | food = Maybe.withDefault 0 (pick food (Model.getFreeLocations model.snake)) }
            , Cmd.none
            )
        Start ->
            ( { model | status = Model.Running }
            , Cmd.none
            )
        Restart ->
            Model.init ()
        GameOver ->
            ( { model | status = Model.Ended }
            , Cmd.none
            )
        Step -> 
            updateStep model
        KeyDown key ->
            updateKeyDown key model
        ButtonClick d ->
            updateDirection (Just d) model


updateKeyDown : String -> Model -> (Model, Cmd Msg)
updateKeyDown key model =
    case model.status of
        Model.Initial ->
            updateDifficulty (numberParser key) model
        Model.Ready ->
            update Model.Start model
        Model.Running ->
            updateDirection (directionParser key) model
        Model.Ended -> 
            case directionParser key of
                Nothing ->
                    update Model.Restart model
                Just _ ->
                    ( model, Cmd.none )

updateDifficulty : Maybe Int -> Model -> (Model, Cmd Msg)
updateDifficulty d model =
    case d of 
        Just n -> 
            ( { model 
                | difficulty = n
                , status = Ready
                , snake = List.reverse (List.range (centerLocation - 1) (centerLocation + 1))
                , food = centerLocation + 5 
                }
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

updateStep : Model -> (Model, Cmd Msg)
updateStep model =
    if model.nextStep == 0 then
        updateSnake model
    else
        ({ model | nextStep = model.nextStep - 1}
        , Cmd.none
        )

updateSnake : Model -> (Model, Cmd Msg)
updateSnake model = 
    let
        head = Maybe.map (\l -> Model.step l model.direction) (List.head model.snake)
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

--- UTILITY

pick : Int -> Set a -> Maybe a
pick idx set = 
    Array.get idx (Array.fromList (Set.toList set))
