module Main exposing (main)
import Browser
import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes as SAt
import Time exposing (Posix)
type alias Pos = (Float, Float, Float)


dt (x,y,z) =
  let
    dx = 10*(y-x)
    dy = 28*x-y-x*z
    dz = x*y-8/3*z
  in
    (dx,dy,dz)

initPos =
  (0.5,1,1)

trajectoryLength = 300

tickSpeed = 200 --[ticks/sec]



-- MODEL

type alias Model =
  { previousPos : List Pos
  , currentPos : Pos
  }

-- INIT

init : Model
init =
  { previousPos = []
  , currentPos = initPos
  }

-- UPDATE

type Msg
  = Tick Posix

plus (x,y,z) (dx,dy,dz) =
  (x+dx*(1/tickSpeed), y+dy*(1/tickSpeed), z+dz*(1/tickSpeed))

update : Msg -> Model -> Model
update msg model =
  case msg of
    Tick _ ->
      let
        newPreviousPos =
          case model.previousPos of
            hd :: tl ->
              if List.length model.previousPos > trajectoryLength
              then
                tl ++ [model.currentPos]
              else
                model.previousPos ++ [model.currentPos]
            _ ->
              [model.currentPos]
      in
        { previousPos = newPreviousPos
        , currentPos = plus model.currentPos (dt model.currentPos)
        }

-- VIEW

view : Model -> Html Msg
view model =
  svg
    [ SAt.width  "100"
    , SAt.height "100"
    , SAt.viewBox "0 0 100 100"
    ]
    ( List.append
        ( model.previousPos
            |> List.indexedMap (\n pos -> createCircleXY pos False (toFloat n/trajectoryLength))
        )
        [createCircleXY model.currentPos True 1]
    )

createCircleXY (x,y,z) isCurrentPos opacity =
  Svg.circle
    [ SAt.cx <| f2s (x+25)
    , SAt.cy <| f2s (y+25)
    , SAt.r "1"
    , SAt.fill (if isCurrentPos then "red" else "black")
    , SAt.fillOpacity <| f2s opacity
    ]
    []

f2s = String.fromFloat


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> (init, Cmd.none)
    , view = view
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = \_ -> Time.every (1000/tickSpeed) Tick
    }