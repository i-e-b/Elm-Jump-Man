import Keyboard
import Window
import Dict

-- Game entities
type Mario = {x:Float, y:Float, vx:Float, vy:Float, dir:String, jumpEnergy:Int}
type Controls = (Float, {x:Int, y:Int})

-- First try at level structure: a Dict of x locations, each with a list of floor heights.
-- Later, this should load levels over HTTP, and be a signal going into the game.
-- Worlds are blocky, and each position relates to a block index.
-- All blocks are the same size (blockScale)
type World = Dict.Dict Int [Int]
world = Dict.fromList [(0,[3,1]),(1,[2])]
blockScale = 16.0
halfBlockScale = 8.0

-- Structural classes
type Positional a = {a | x:Float, y:Float}

-- MODEL
maxJump = 7
mario : Mario
mario = { x=0, y=0, vx=0, vy=0, dir="right", jumpEnergy=maxJump}

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
floorLevel m =
    let blockX = truncate (m.x / blockScale)
        heights = map (\h->h*blockScale) (Dict.findWithDefault [] blockX world) ++ [0]
    in  head (filter (\h -> h <= m.y || h == 0) heights)

nearEnough : Positional a -> Positional b -> Bool
nearEnough u v = abs (u.x - v.x) < halfBlockScale && abs (u.y - v.y) < halfBlockScale

-- should be able to jump when on a surface (or enemy)
-- can moderate height of jump by duration of 'up' press
jump {y} m = if
    | y > 0 && (onSurface m || m.jumpEnergy > 0) -> { m | vy <- 4, jumpEnergy <- m.jumpEnergy - 1 }
    | y <= 0 && not (onSurface m) -> {m | jumpEnergy <- 0}
    | otherwise -> m

-- fall unless sitting on a surface
gravity t m = if not (onSurface m) then { m | vy <- m.vy - t/4 } else {m | jumpEnergy <- maxJump}

-- apply acceleration and constraints
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
  in collage w' h' (
        renderBackground (w,h) world
        ++
        [toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)]
        ++
        renderBlocks (w,h) world
        )

renderBlocks : (Float, Float) -> World -> [Form]
renderBlocks (w',h') w = {- hard coded to start -}
    let block x y = toForm (image 16 16 "/imgs/world/smash-block.png") 
                    |> move ((blockScale * x), (blockScale * y) + 40 - h'/2)
        allBlocks = concatMap (\(x, ys) -> map (\y -> block (toFloat x) (toFloat y)) ys) (Dict.toList w)
    in  allBlocks

renderBackground : (Float,Float) -> World -> [Form]
renderBackground (w,h) wr = 
    [ rect w h  |> filled (rgb 174 238 238)
    , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
    ]

-- Hooking everything up...
input : Signal Controls
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main : Signal Element
main  = lift2 render Window.dimensions (foldp step mario input)

