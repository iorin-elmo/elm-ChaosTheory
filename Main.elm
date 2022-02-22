module Main exposing (main)
import Browser
import Html exposing (Html)
import Html.Attributes as H
import Html.Events as Ev
import Svg exposing (svg)
import Svg.Attributes as SAt
import Time exposing (Posix)
type alias Pos = (Float, Float, Float)




initPos =
  (-10.6,-4.4,28.6)

trajectoryLength = 1000

tickSpeed = 640 --[ticks/sec]

selectedAttractor = lorenz



-- MODEL

lorenz (x,y,z) =
  let
    step = 0.002
    dx = 10*(y-x)
    dy = 28*x-y-x*z
    dz = x*y-8/3*z
  in
    (dx*step,dy*step,dz*step)

chen (x,y,z) =
  let
    step = 0.0001
    dx = 400*(y-x)
    dy = -120*x-10*x*z+280*y
    dz = 10*x*y-30*z
  in
    (dx*step,dy*step,dz*step)

rossler (x,y,z) =
  let
    step = 0.00014
    dx = -500*(y+z)
    dy = 500*x+50*y
    dz = 50+500*z*(x-14)
  in
    (dx*step,dy*step,dz*step)

type alias Model =
  { previousPos : List Pos
  , currentPos : Pos
  , rotateX : Float
  , rotateY : Float
  , scale : Float
  }

-- INIT

init : Model
init =
  { previousPos = []
  , currentPos = initPos
  , rotateX = 0
  , rotateY = 0
  , scale = 10
  }

-- UPDATE

type Msg
  = Tick Posix
  | RotateX String
  | RotateY String
  | Scale String

plus (x,y,z) (dx,dy,dz) =
  (x+dx, y+dy, z+dz)

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
        { model
        | previousPos = newPreviousPos
        , currentPos =
            plus
              model.currentPos
              (selectedAttractor model.currentPos)
        }

    RotateX str ->
      { model
      | rotateX =
          String.toFloat str
            |> Maybe.withDefault model.rotateX
      }

    RotateY str ->
      { model
      | rotateY =
          String.toFloat str
            |> Maybe.withDefault model.rotateY
      }

    Scale str ->
      { model
      | scale =
          String.toFloat str
--            |> Maybe.map (\s -> 10^s)
            |> Maybe.withDefault model.scale
      }



getX (x,y,z) = x
getY (x,y,z) = y
getZ (x,y,z) = z

-- VIEW

view : Model -> Html Msg
view model =
  Html.div []
  [ svg
    [ SAt.width  "1000"
    , SAt.height "800"
    , SAt.viewBox "0 0 1000 800"
    ]
    ( model.previousPos
      |> List.indexedMap
        (\n pos ->
          let
            mat =
              rotateX (degrees model.rotateX)
                |> multiply (rotateY (degrees model.rotateY))
                |> multiply (scale model.scale)
            rotatedPos =
              pos
                |> linearMap mat
            movedPos =
              plus
                rotatedPos
                ( 25*model.scale
                , 25*model.scale
                , 25*model.scale
                )
          in
            createCircle movedPos (toFloat n/trajectoryLength)
        )
    )
  , createSlider 0 180 model.rotateX RotateX
  , createSlider 0 90 model.rotateY RotateY
  , createSlider 1 20 model.scale Scale
  ]

createCircle (x,y,z) opacity =
  Svg.circle
    [ SAt.cx <| f2s (x+25)
    , SAt.cy <| f2s (y+25)
    , SAt.r "1"
    , SAt.fill "black"
    , SAt.fillOpacity <| f2s opacity
    ]
    []

createSlider min max value msg =
  Html.div []
    [ Html.input
      [ H.type_ "range"
      , H.max <| f2s max
      , H.min <| f2s min
      , H.value <| f2s value
      , Ev.onInput msg
      ][]
    , f2s value
        |> Html.text
    ]

f2s = String.fromFloat

-- UTILITIES

type alias Matrix =
  (Pos, Pos, Pos)

multiply : Matrix -> Matrix -> Matrix
multiply ((a11,a12,a13),(a21,a22,a23),(a31,a32,a33)) ((b11,b12,b13),(b21,b22,b23),(b31,b32,b33)) =
  ( (a11*b11+a12*b21+a13*b31, a11*b12+a12*b22+a13*b32, a11*b13+a12*b23+a13*b33)
  , (a21*b11+a22*b21+a23*b31, a21*b12+a22*b22+a23*b32, a21*b13+a22*b23+a23*b33)
  , (a31*b11+a32*b21+a33*b31, a31*b12+a32*b22+a33*b32, a31*b13+a32*b23+a33*b33)
  )

linearMap : Matrix -> Pos -> Pos
linearMap ((a11,a12,a13),(a21,a22,a23),(a31,a32,a33)) (x,y,z) =
  ( a11*x + a12*y + a13*z
  , a21*x + a22*y + a23*z
  , a31*x + a32*y + a33*z
  )


rotateX : Float -> Matrix
rotateX theta =
  ( (1,         0,            0)
  , (0, cos theta, -(sin theta))
  , (0, sin theta,   cos theta )
  )

rotateY : Float -> Matrix
rotateY theta =
  ( (   cos theta, 0, sin theta)
  , (           0, 1,         0)
  , (-(sin theta), 0, cos theta)
  )

rotateZ : Float -> Matrix
rotateZ theta =
  ( ( cos theta, -(sin theta), 0)
  , ( sin theta,    cos theta, 0)
  , (         0,            0, 1)
  )

scale : Float -> Matrix
scale k =
  ( (k,0,0)
  , (0,k,0)
  , (0,0,k)
  )

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> (init, Cmd.none)
    , view = view
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = \_ -> Time.every (1000/tickSpeed) Tick
    }