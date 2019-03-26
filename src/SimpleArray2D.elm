module SimpleArray2D exposing
    ( SimpleArray2D
    , get
    , getWithDefault
    , indexedFoldl
    , repeat
    , set
    )

import Array exposing (Array)


type SimpleArray2D t
    = SimpleArray2DInstance Int (Array t)



-- Basic Functions --


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
get coord (SimpleArray2DInstance numColumns array1d) =
    Array.get (columnRowToIndex numColumns coord) array1d



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
            remainderBy numColumns index

        row =
            index // numColumns
    in
    ( column, row )


columnRowToIndex : Int -> ( Int, Int ) -> Int
columnRowToIndex numColumns ( column, row ) =
    column + numColumns * row
