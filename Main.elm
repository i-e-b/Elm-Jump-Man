import Keyboard
import Window
import Dict
import open Sort

-- Game entities
type Denizen a = {a | x:Float, y:Float, vx:Float, vy:Float, dir:Float, jumpEnergy:Int}
type Mario = Denizen ({})
type IVec = {x:Int, y:Int}
type Controls = (Float, IVec)
type Time = Float
type Enemy = Denizen ({})

-- Worlds are blocky, and each position relates to a block index.
-- All blocks are the same size (blockScale)
type World = Dict.Dict Int [Int]
defaultWorld = Dict.fromList [
    (-8,[4]),(-6,[1,2,3,4,5,6])
    ,(-3,[4]),(1,[10,3,1]),(2,[9,2])
    ,(3,[8,3]),(4,[4]),(5,[5,1])
    ,(6,[6,1]),(7,[6,1]),(8,[5,1])
    ]

blockScale = 16.0
halfBlockScale = 8.0
maxHeight = 65000.0 {- maximum height of a world -}

-- Structural classes
type Positional a = {a | x:Float, y:Float}

-- Game state
type Scene = {world:World, mario:Mario, enemies:[Enemy]}
scene_world_1_1 = {world = defaultWorld, mario = defaultMario, enemies = []}

-- MODEL
maxJump = 7
defaultMario = { x=0, y=0, vx=0, vy=0, dir=1, jumpEnergy=maxJump}
marioHead m = m.y + 28

{- To do:
 - enemies and death
 - squishing enemies
 - bumping blocks from under-side
 - going down pipes
 - scenes, file loading?
 -}

-- are we standing on a surface? If not, we are in free fall (possibly jumping)
onSurface : Scene -> Mario -> Bool
onSurface sc m = (m.y <= floorLevel sc m)

-- all floors in mario's x position
heights : Scene -> Float -> [Float]
heights sc x =
    let s = 2 {- how much smaller mario is than a block -}
        blockA = ceiling ((x - s) / blockScale)   {- we can possibly overlap 2 blocks at a time -}
        blockB = floor ((x + s) / blockScale)     {- so we find both and take the highest below mario -}
        blockList = Dict.findWithDefault [] (blockA) sc.world
                 ++ Dict.findWithDefault [] (blockB) sc.world
    in  map (\h-> toFloat h * blockScale) blockList

-- nearest floor below us
floorLevel : Scene -> Mario -> Float
floorLevel sc m =
    let stepHeight = m.y + 4.0 {- mario can step up a bit. Makes the game feel smoother -}
        allHeights = heights sc m.x
    in  maximum ((filter (\h -> h <= stepHeight) allHeights)++[0])

-- nearest floor above, minus block height
ceilingLevel : Scene -> Mario -> Float
ceilingLevel sc m = minimum (filter (\h -> h > m.y) (map (\h -> h - blockScale) (heights sc m.x)++[maxHeight]))

-- should be able to jump when on a surface (or enemy)
-- can moderate height of jump by duration of 'up' press
jump : IVec -> Scene -> Denizen {} -> Denizen {}
jump {y} sc m = 
        if | y > 0 && (onSurface sc m || m.jumpEnergy > 0) -> {m| vy <- 4, jumpEnergy <- m.jumpEnergy - 1}
           | y <= 0 && not (onSurface sc m) -> {m | jumpEnergy <- 0}
           | otherwise -> m

-- fall unless sitting on a surface
gravity t sc m = if not (onSurface sc m) then { m | vy <- m.vy - t/4 } else {m | jumpEnergy <- maxJump}


-- return true if mario can't move to new position
blockedX : Scene -> Mario -> Bool
blockedX s m =
    let mh = {m | y <- if (m.vx <= 0) then (marioHead m) else (m.y + blockScale)}
        constrainedWidth = (floorLevel s m /= floorLevel s mh) || (ceilingLevel s m /= ceilingLevel s mh)
    in  constrainedWidth && (m.vx /= 0)

blockedY : Scene -> Mario -> Bool
blockedY s m = (marioHead m > ceilingLevel s m) && m.vy > 0

-- apply acceleration and constraints
physics : Time -> Scene -> Denizen {} -> Denizen {}
physics t s m = 
    let dx = t * m.vx
        dy = t * m.vy

        wall = blockedX s {m | x <- m.x + dx}
        ceil = blockedY s m
        stuck = (wall && blockedX s m) && (m.vy == 0)

        vy' = if (ceil) then 0 else m.vy
        je' = if (ceil) then 0 else m.jumpEnergy
        vx' = if wall then 0 else m.vx
        x' = if (stuck) then (m.x - t * m.vx) else m.x + t * vx'
        y' = min (max (floorLevel s m) (m.y + t * vy')) ((ceilingLevel s m)-24)

        vy'' = if (m.y == y') then 0 else vy'
    in  { m | x <- x', y <- max 0 y', vy <- vy'', vx <- vx', jumpEnergy <- je'}

-- apply walking (side-to-side) control, set direction for graphics
--walk : {x,y} -> Mario -> Mario
walk {x} m = { m | vx <- toFloat x, dir <- if (x ==0) then m.dir else toFloat x}

-- Apply all the things!
step : Controls -> Scene -> Scene
step (t,dir) scene = 
    let controlMario = walk dir . jump dir scene
        updateDenizen = physics t scene . gravity t scene
    in {scene | mario <- updateDenizen(controlMario scene.mario)}


-- DISPLAY
render : (Int, Int) -> Scene -> Element 
render (w',h') scene =
    let (w,h) = (toFloat w', toFloat h')
        mario = scene.mario
        verb = if | not (onSurface scene mario) -> "jump"
                  | mario.vx /= 0 -> "walk"
                  | otherwise     -> "stand"
        dir = if (mario.dir > 0) then "right" else "left"
        src  = "/imgs/mario/" ++ verb ++ "-" ++ dir ++ ".gif"
    in 
--        (asText mario) `above`
        collage w' h' (
            renderBackground (w,h) scene.world
            ++
            [toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)]
            ++
            renderBlocks (w,h) scene.world
            )

renderBlocks : (Float, Float) -> World -> [Form]
renderBlocks (w',h') w = {- hard coded to start -}
    let block x y = toForm (image 16 16 "/imgs/world/smash-block.png") 
                    |> move ((blockScale * x), (blockScale * y) + 40 - h'/2)
    in  concatMap (\(x, ys) -> map (\y -> block (toFloat x) (toFloat y)) ys) (Dict.toList w)

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
main = lift2 render Window.dimensions (foldp step scene_world_1_1 input)

