module Setup exposing (Setup, cellSize, setup, thickness)

import ExampleWorlds
import Types exposing (Miliseconds)


setup : Setup
setup =
    { tickTime = 100
    , height = 500
    , width = 500
    , gridSize = 20

    -- , initialAliveCellList = ExampleWorlds.initialStateGlider
    -- , initialAliveCellList = ExampleWorlds.initialStateOne
    , initialAliveCellList = ExampleWorlds.initialStateTwo
    }


type alias Setup =
    { tickTime : Miliseconds
    , height : Float
    , width : Float
    , gridSize : Int
    , initialAliveCellList : List ExampleWorlds.Cell
    }


thickness : Float
thickness =
    cellSize * 0.5


cellSize : Float
cellSize =
    setup.width / toFloat setup.gridSize
