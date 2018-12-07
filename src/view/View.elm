module View exposing (view)

import Model exposing (..)
import Set exposing (Set)
import Html exposing (Html, pre, div, text, button, table, tr, td)
import Html.Events exposing (onClick)
import Html.Attributes exposing (tabindex, style)

view : Model -> Html Msg 
view model = 
    div 
        [ tabindex 0
        , style "outline" "none"
        , style "text-align" "center"
        ] 
        [ viewHeader
        , viewBody model
        , viewFooter model
        ]

viewHeader : Html Msg
viewHeader = 
    pre [style "font-family" "monospace"] [ text viewHeadline ]

viewHeadline : String 
viewHeadline =
    "     __  _       _  _  ____ \n" ++
    " ___|  \\| |_____| |/ /| ___|\n" ++
    "| __|     |  _  |   / | __| \n" ++
    "|__ |_|\\__| |_| |   \\ |____|\n" ++
    "|___|     |  _  |_|\\_\\      \n" ++
    "          |_| |_|           \n"

viewBody : Model -> Html Msg 
viewBody model =
    case model.status of 
        Initial ->
            viewDifficultyKeys
        _ -> 
            viewBoard model

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
