module ExampleWorlds exposing (Cell, initialStateGlider, initialStateOne, initialStateTwo)


type alias Cell =
    { x : Int
    , y : Int
    }


initialStateOne =
    [ Cell 0 0
    , Cell 0 1
    , Cell 1 1
    , Cell 2 2
    , Cell 3 3
    , Cell 4 2
    , Cell 2 4
    , Cell 4 4
    , Cell 5 4
    , Cell 4 5
    , Cell 5 5
    ]


initialStateTwo =
    [ -- Cell 0 0
      -- ,
      Cell 0 1
    , Cell 1 1
    , Cell 2 2
    , Cell 3 3
    , Cell 4 2
    , Cell 2 4
    , Cell 4 4
    , Cell 5 4
    , Cell 4 5
    , Cell 5 5
    ]


initialStateGlider =
    [ Cell 0 1
    , Cell 1 2
    , Cell 2 0
    , Cell 2 1
    , Cell 2 2
    ]
