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
    = Alive
    | Dead


initWorld : World
initWorld =
    { cells =
        List.foldl (\{ x, y } cells -> SimpleArray2D.set ( x, y ) Alive cells)
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
                Alive ->
                    renderItem ( x, y ) :: list

                Dead ->
                    list
        )
        []
        world.cells


renderItem : ( Int, Int ) -> Canvas.Renderable
renderItem ( col, row ) =
    let
        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( (rowf * Setup.cellSize) + Setup.cellSize
            , (colf * Setup.cellSize) + Setup.cellSize
            )
    in
    Canvas.shapes
        [ Canvas.fill Color.black ]
        [ Canvas.circle ( x - Setup.thickness, y - Setup.thickness ) Setup.thickness ]


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
                    | cells =
                        world.cells
                    , lastTime = time
                  }
                , Cmd.none
                )

        SetRunning isRunning ->
            ( { world | isRunning = isRunning }, Cmd.none )
