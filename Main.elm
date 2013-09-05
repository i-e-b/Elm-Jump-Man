import Keyboard
import Window

type Mario = {x:Float, y:Float, vx:Float, vy:Float, dir:String, jumpFrames:Int}
type Controls = (Float, {x:Int, y:Int})
type Block = {x:Float, y:Float}
type Wall = [Block]

type Positional a = {a | x:Float, y:Float}

-- MODEL
mario : Mario
mario = { x=0, y=0, vx=0, vy=0, dir="right", jumpFrames=0}

world : Wall
world = [{x=0, y=4}]

{- To do:
 - blocks & floors, jumping
 - enemies and death
 - squishing enemies
 - bumping blocks from under-side
 - going down pipes
 - scenes, file loading?
 -}

-- are we standing on a surface? If not, we are in free fall (possibly jumping)
onSurface : Mario -> Bool
onSurface m = (m.y <= floorLevel m)

-- nearest floor below us
floorLevel : Mario -> Float
floorLevel m = if 
    | (nearEnough m {x=0, y=20}) -> 20
    | otherwise -> 0


nearEnough : Positional a -> Positional b -> Bool
nearEnough u v = abs (u.x - v.x) < 10 && abs (u.y - v.y) < 10

-- should be able to jump when on a surface (or enemy)
-- can moderate height of jump by duration of 'up' press
jump {y} m = if y > 0 && (onSurface m || m.jumpFrames < 7) then { m | vy <- 4, jumpFrames <- m.jumpFrames + 1 } else m

-- gravity needs to stop on surface rather than zero
gravity t m = if not (onSurface m) then { m | vy <- m.vy - t/4 } else {m | jumpFrames <- 0}

-- these two are probably ok for now.
physics t m = { m | x <- m.x + t*m.vx , y <- max (floorLevel m) (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

-- Apply all the things!
step : Controls -> Mario -> Mario
step (t,dir) = physics t . walk dir . gravity t . jump dir


-- DISPLAY
render : (Int, Int) -> Mario -> Element 
render (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | mario.y  >  floorLevel mario -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src  = "/imgs/mario/" ++ verb ++ "-" ++ mario.dir ++ ".gif"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
      ]

-- MARIO
input : Signal Controls
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main : Signal Element
main  = lift2 render Window.dimensions (foldp step mario input)

