import Keyboard
import Window
import Dict
import open Sort

-- Game entities
type Denizen a = {a | x:Float, y:Float, vx:Float, vy:Float, dir:String}
type Mario = Denizen ({jumpEnergy:Int})
type Controls = (Float, {x:Int, y:Int})
type Time = Float
type Enemy = Denizen ({})

-- Worlds are blocky, and each position relates to a block index.
-- All blocks are the same size (blockScale)
type World = Dict.Dict Int [Int]
world = Dict.fromList [(-3,[4]),(1,[10,3,1]),(2,[9,2]),(3,[8,3]),(4,[4]),(5,[5,1]),(6,[6,1]),(7,[6,1]),(8,[5,1])]
blockScale = 16.0
halfBlockScale = 8.0
maxHeight = 65000.0 {- maximum height of a world -}

-- Structural classes
type Positional a = {a | x:Float, y:Float}

-- Game state
type Scene = {world:World, mario:Mario, enemies:[Enemy]}
scene_world_1_1 = {world = world, mario = mario, enemies = []}

-- MODEL
maxJump = 7
mario : Mario
mario = { x=0, y=0, vx=0, vy=0, dir="right", jumpEnergy=maxJump}
marioHead m = m.y + 28

{- To do:
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
    let s = if (m.vy == 0) then 4 else 2 {- much much smaller mario is than a block -}
        blockA = ceiling ((m.x - s) / blockScale)   {- we can possibly overlap 2 blocks at a time -}
        blockB = floor ((m.x + s) / blockScale)     {- so we find both and take the highest below mario -}
        blockList = Dict.findWithDefault [] (blockA) world
                 ++ Dict.findWithDefault [] (blockB) world
    in  map (\h-> h * blockScale) blockList

-- nearest floor below us
floorLevel : Mario -> Float
floorLevel m = maximum ((filter (\h -> h <= m.y) (heights m))++[0])

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


-- given old mario, new mario, return true if mario can't move to new position
blockedX : Mario -> Bool
blockedX dm =
    let dmh = {dm | y <- if (dm.vx <= 0) then (marioHead dm) else (dm.y + blockScale)}
        constrainedWidth = 
            (floorLevel dm /= floorLevel dmh) || (ceilingLevel dm /= ceilingLevel dmh)
    in  constrainedWidth && (dm.vx /= 0)

blockedY : Mario -> Bool
blockedY m = (marioHead m > ceilingLevel m) && m.vy > 0

-- apply acceleration and constraints
physics : Time -> Mario -> Mario
physics t m = 
    let dx = t * m.vx
        dy = t * m.vy
        dm = {m | x <- m.x + dx, y <- m.y - dy} {- proposed new mario location -}

        wall = blockedX dm
        ceil = blockedY m

        vy' = if (ceil) then 0 else m.vy
        je' = if (ceil) then 0 else m.jumpEnergy
        vx' = if (wall) then 0 else m.vx
        x' = m.x + t * vx'
        y' = min (max (floorLevel m) (m.y + t * vy')) ((ceilingLevel m)-24)

        vy'' = if (m.y == y') then 0 else vy'
    in  { m | x <- x', y <- max 0 y', vy <- vy'', vx <- vx', jumpEnergy <- je'}

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
      verb = if | not (onSurface mario) -> "jump"
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

