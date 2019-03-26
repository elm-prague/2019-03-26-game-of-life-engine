module Game exposing (Msg(..), World, initWorld, update, view)

import Canvas
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Setup exposing (setup)
import SimpleArray2D exposing (SimpleArray2D)
import Types exposing (Miliseconds)



-- World --


type alias World =
    { cells : SimpleArray2D CellState
    , isRunning : Bool
    , lastTime : Miliseconds
    }


type CellState
    = Alive Int
    | Dead


initWorld : World
initWorld =
    { cells =
        List.foldl (\{ x, y } cells -> SimpleArray2D.set ( x, y ) (Alive 0) cells)
            (SimpleArray2D.repeat setup.gridSize setup.gridSize Dead)
            setup.initialAliveCellList
    , isRunning = True
    , lastTime = 0
    }



-- Msg --


type Msg
    = Tick Miliseconds
    | SetRunning Bool



-- Util --


normalizeCoord : ( Int, Int ) -> ( Int, Int )
normalizeCoord =
    Tuple.mapBoth
        (modBy setup.gridSize)
        (modBy setup.gridSize)



-- View --


view : World -> Html Msg
view world =
    Canvas.toHtml ( round setup.width, round setup.height )
        [ Html.Attributes.style "width" <| toPx setup.width
        , Html.Attributes.style "height" <| toPx setup.height
        , Html.Events.onClick <| SetRunning (not world.isRunning)
        ]
        (Canvas.shapes [ Canvas.fill Color.white ]
            [ Canvas.rect ( 0, 0 ) setup.width setup.height
            ]
            :: renderItems world
            ++ [ Canvas.text [ Canvas.fill Color.red, Canvas.font { size = 15, family = "Arial Black" } ] ( 10, 20 ) <|
                    if world.isRunning then
                        "Running..."

                    else
                        "Paused"
               ]
        )


renderItems : World -> List Canvas.Renderable
renderItems world =
    SimpleArray2D.indexedFoldl
        (\( x, y ) cellState list ->
            case cellState of
                Alive age ->
                    renderItem ( x, y ) age :: list

                Dead ->
                    list
        )
        []
        world.cells


renderItem : ( Int, Int ) -> Int -> Canvas.Renderable
renderItem ( col, row ) age =
    let
        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( (rowf * Setup.cellSize) + Setup.cellSize
            , (colf * Setup.cellSize) + Setup.cellSize
            )

        colorComponent =
            min (toFloat age / 10.0) 0.9

        color =
            Color.rgb colorComponent colorComponent colorComponent
    in
    Canvas.shapes
        [ Canvas.fill color ]
        [ Canvas.circle ( x - Setup.thickness, y - Setup.thickness ) (Setup.thickness * (colorComponent + 1)) ]


toPx : Float -> String
toPx value =
    String.fromFloat value ++ "px"



-- Update --


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick time ->
            if time - world.lastTime < setup.tickTime then
                ( world, Cmd.none )

            else
                ( { world
                    | cells = updateCells world.cells
                    , lastTime = time
                  }
                , Cmd.none
                )

        SetRunning isRunning ->
            ( { world | isRunning = isRunning }, Cmd.none )


updateCells : SimpleArray2D CellState -> SimpleArray2D CellState
updateCells simpleArray =
    let
        cellsNeighbors =
            SimpleArray2D.indexedMap (getCellsNeighbors simpleArray) simpleArray
    in
    SimpleArray2D.indexedMap gameRule cellsNeighbors


getCellsNeighbors : SimpleArray2D CellState -> ( Int, Int ) -> CellState -> ( CellState, List CellState )
getCellsNeighbors cells ( i, j ) item =
    let
        dirs =
            [ -1, 0, 1 ]

        indices =
            dirs
                |> List.concatMap (\a -> dirs |> List.map (\b -> ( a + i, b + j )))
                |> List.filter ((/=) ( i, j ))
    in
    ( item
    , List.filterMap
        (\coords ->
            SimpleArray2D.get (normalizeCoord coords) cells
        )
        indices
    )


gameRule ( i, j ) ( cell, neighbours ) =
    let
        aliveNeighbours =
            List.Extra.count
                (\c ->
                    case c of
                        Alive _ ->
                            True

                        Dead ->
                            False
                )
                neighbours
    in
    case ( cell, aliveNeighbours ) of
        ( Alive 50, _ ) ->
            Dead

        ( Alive age, 2 ) ->
            Alive <| age + 1

        ( Alive age, 3 ) ->
            Alive <| age + 1

        ( Dead, 3 ) ->
            Alive 0

        _ ->
            Dead
