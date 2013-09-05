import Keyboard
import Window
import Dict
import open Sort

-- Game entities
type Mario = {x:Float, y:Float, vx:Float, vy:Float, dir:String, jumpEnergy:Int}
type Controls = (Float, {x:Int, y:Int})
type Time = Float

-- First try at level structure: a Dict of x locations, each with a list of floor heights.
-- Later, this should load levels over HTTP, and be a signal going into the game.
-- Worlds are blocky, and each position relates to a block index.
-- All blocks are the same size (blockScale)
-- height lists MUST be in descending order
type World = Dict.Dict Int [Int]
world = Dict.fromList [(-3,[4]),(1,[10,3,1]),(2,[9,2]),(3,[8,3]),(4,[4]),(5,[5,1]),(6,[6,1]),(7,[6,1]),(8,[6,1])]
blockScale = 16.0
halfBlockScale = 8.0
maxHeight = 65000.0 {- maximum height of a world -}

-- Structural classes
type Positional a = {a | x:Float, y:Float}

-- MODEL
maxJump = 7
mario : Mario
mario = { x=0, y=0, vx=0, vy=0, dir="right", jumpEnergy=maxJump}
marioHead m = m.y + 28

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

-- all floors in mario's x position
heights : Mario -> [Float]
heights m = 
    let blockA = ceiling ((m.x - 2) / blockScale)   {- we can possibly overlap 2 blocks at a time -}
        blockB = floor ((m.x + 2) / blockScale)     {- so we find both and take the highest below mario -}
        blockList = Dict.findWithDefault [] (blockA) world
                 ++ Dict.findWithDefault [] (blockB) world
    in  map (\h-> h * blockScale) blockList

-- nearest floor below us
floorLevel : Mario -> Float
floorLevel m = maximum (filter (\h -> h <= m.y || h <= 0) (heights m)++[0])

-- nearest floor above, minus block height
ceilingLevel : Mario -> Float
ceilingLevel m = minimum (filter (\h ->h>m.y) (map (\h->h - blockScale) (heights m)++[maxHeight]))

-- should be able to jump when on a surface (or enemy)
-- can moderate height of jump by duration of 'up' press
jump {y} m = if
    | y > 0 && (onSurface m || m.jumpEnergy > 0) -> { m | vy <- 4, jumpEnergy <- m.jumpEnergy - 1 }
    | y <= 0 && not (onSurface m) -> {m | jumpEnergy <- 0}
    | otherwise -> m

-- fall unless sitting on a surface
gravity t m = if not (onSurface m) then { m | vy <- m.vy - t/4 } else {m | jumpEnergy <- maxJump}

-- apply acceleration and constraints
physics : Time -> Mario -> Mario
physics t m = 
    let constrainedHeight = (marioHead m > ceilingLevel m) && m.vy > 0
        x' = m.x + t*m.vx
        vy' = if (constrainedHeight) then 0 else m.vy
        je' = if (constrainedHeight) then 0 else m.jumpEnergy
        y' = min (max (floorLevel m) (m.y + t*m.vy)) ((ceilingLevel m)-24)
    in  { m | x <- x', y <- y', vy <- vy', jumpEnergy <- je'}

-- apply walking (side-to-side) control, set direction for graphics
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
  in 
--    (asText mario) `above`
    collage w' h' (
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

