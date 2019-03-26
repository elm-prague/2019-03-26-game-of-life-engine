module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Game
import Html exposing (Html)
import SimpleArray2D exposing (SimpleArray2D)
import Time exposing (Posix)
import Types exposing (Miliseconds)



-- Model --


type alias Model =
    { world : Game.World
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { world = Game.initWorld
      }
    , Cmd.none
    )



-- Msg --


type Msg
    = AnimationFrame Posix
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame t ->
            let
                timeMiliseconds =
                    Time.posixToMillis t

                worldMsg =
                    Game.Tick timeMiliseconds

                ( newWorld, cmd ) =
                    Game.update worldMsg model.world
            in
            ( { model | world = newWorld }, Cmd.none )

        GameMsg gameMsg ->
            let
                ( newWorld, cmd ) =
                    Game.update gameMsg model.world
            in
            ( { model | world = newWorld }, Cmd.none )


view : Model -> Html Msg
view { world } =
    Html.div []
        [ Html.text "Game of Life?"
        , Html.map GameMsg <|
            Game.view world
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions { world } =
    if world.isRunning then
        onAnimationFrame AnimationFrame

    else
        Sub.none
