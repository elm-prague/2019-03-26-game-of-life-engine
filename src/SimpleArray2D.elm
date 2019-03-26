module SimpleArray2D exposing
    ( SimpleArray2D
    , get
    , getWithDefault
    , indexedFoldl
    , indexedMap
    , repeat
    , set
    )

import Array exposing (Array)


type SimpleArray2D t
    = SimpleArray2DInstance Int (Array t)



-- Basic Functions --


indexedMap : (( Int, Int ) -> a -> b) -> SimpleArray2D a -> SimpleArray2D b
indexedMap func (SimpleArray2DInstance numColumns array1d) =
    let
        func1d index item =
            func (indexToColumnRow numColumns index) item

        newArray1d =
            Array.indexedMap func1d array1d
    in
    SimpleArray2DInstance numColumns newArray1d


indexedFoldl : (( Int, Int ) -> a -> b -> b) -> b -> SimpleArray2D a -> b
indexedFoldl func baseCase (SimpleArray2DInstance numColumns array1d) =
    let
        func1d item ( index, aggregate ) =
            ( index + 1
            , func (indexToColumnRow numColumns index) item aggregate
            )

        ( _, finalAggregate ) =
            Array.foldl func1d ( 0, baseCase ) array1d
    in
    finalAggregate


repeat : Int -> Int -> a -> SimpleArray2D a
repeat numColumns numRows initialElement =
    let
        columns =
            max 1 numColumns

        rows =
            max 1 numRows

        array =
            Array.repeat (columns * rows) initialElement
    in
    SimpleArray2DInstance numColumns array


set : ( Int, Int ) -> a -> SimpleArray2D a -> SimpleArray2D a
set coord newValue (SimpleArray2DInstance numColumns array1d) =
    let
        newArray1d =
            Array.set (columnRowToIndex numColumns coord) newValue array1d
    in
    SimpleArray2DInstance numColumns newArray1d


get : ( Int, Int ) -> SimpleArray2D a -> Maybe a
get ( x, y ) (SimpleArray2DInstance numColumns array1d) =
    if y >= (Array.length array1d // numColumns) || y < 0 || x >= numColumns || x < 0 then
        Nothing

    else
        Array.get (columnRowToIndex numColumns ( x, y )) array1d



-- Higher Functions --


getWithDefault : a -> ( Int, Int ) -> SimpleArray2D a -> a
getWithDefault default coord array2d =
    get coord array2d
        |> Maybe.withDefault default



-- Util --


indexToColumnRow : Int -> Int -> ( Int, Int )
indexToColumnRow numColumns index =
    let
        column =
            modBy numColumns index

        row =
            index // numColumns
    in
    ( column, row )


columnRowToIndex : Int -> ( Int, Int ) -> Int
columnRowToIndex numColumns ( column, row ) =
    column + numColumns * row
