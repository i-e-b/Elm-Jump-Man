
module Physics where

import Dict

type IVec = {x:Int, y:Int}
-- Denizens are creatures affected by physics
type Denizen = {x:Float, y:Float, vx:Float, vy:Float, dir:Float, jumpEnergy:Int}

-- Worlds are blocky, and each position relates to a block index.
-- All blocks are the same size (blockScale)
type World = Dict.Dict Int [Int]

-- Game state
type Scene = {world:World, mario:Denizen, enemies:[Denizen]}

-- A few constants
maxJump = 7
marioHead m = m.y + 28
blockScale = 16.0
halfBlockScale = 8.0
maxHeight = 65000.0 {- maximum height of a world -}

-- are we standing on a surface? If not, we are in free fall (possibly jumping)
onSurface : World -> Denizen -> Bool
onSurface sc m = (m.y <= floorLevel sc m)

-- all floors in mario's x position
heights : World -> Float -> [Float]
heights world x =
    let s = 2 {- how much smaller mario is than a block -}
        blockA = ceiling ((x - s) / blockScale)   {- we can possibly overlap 2 blocks at a time -}
        blockB = floor ((x + s) / blockScale)     {- so we find both and take the highest below mario -}
        blockList = Dict.findWithDefault [] (blockA) world
                 ++ Dict.findWithDefault [] (blockB) world
    in  map (\h-> toFloat h * blockScale) blockList

-- nearest floor below us
floorLevel : World -> Denizen -> Float
floorLevel world m =
    let stepHeight = m.y + 4.0 {- mario can step up a bit. Makes the game feel smoother -}
        allHeights = heights world m.x
    in  maximum ((filter (\h -> h <= stepHeight) allHeights)++[0])

-- nearest floor above, minus block height
ceilingLevel : World -> Denizen -> Float
ceilingLevel world m = minimum (filter (\h -> h > m.y) (map (\h -> h - blockScale) (heights world m.x)++[maxHeight]))

-- should be able to jump when on a surface (or enemy)
-- can moderate height of jump by duration of 'up' press
jump : IVec -> Scene -> Denizen -> Denizen
jump {y} sc m = 
        if | y > 0 && (onSurface sc.world m || m.jumpEnergy > 0) -> {m| vy <- 4, jumpEnergy <- m.jumpEnergy - 1}
           | y <= 0 && not (onSurface sc.world m) -> {m | jumpEnergy <- 0}
           | otherwise -> m

-- fall unless sitting on a surface
gravity : Time -> Scene -> Denizen -> Denizen
gravity t sc m = if not (onSurface sc.world m) then { m | vy <- m.vy - t/4 } else {m | jumpEnergy <- maxJump}


-- return true if mario can't move to new position
blockedX : World -> Denizen -> Bool
blockedX s m =
    let mh = {m | y <- if (m.vx <= 0) then (marioHead m) else (m.y + blockScale)}
        constrainedWidth = (floorLevel s m /= floorLevel s mh) || (ceilingLevel s m /= ceilingLevel s mh)
    in  constrainedWidth && (m.vx /= 0)

blockedY : World -> Denizen -> Bool
blockedY s m = (marioHead m > ceilingLevel s m) && m.vy > 0

-- apply acceleration and constraints
physics : Time -> Scene -> Denizen -> Denizen
physics t s m = 
    let dx = t * m.vx
        dy = t * m.vy

        wall = blockedX s.world {m | x <- m.x + dx}
        ceil = blockedY s.world m
        stuck = (wall && blockedX s.world m) && (m.vy == 0)

        vy' = if (ceil) then 0 else m.vy
        je' = if (ceil) then 0 else m.jumpEnergy
        vx' = if wall then 0 else m.vx
        x' = if (stuck) then (m.x - t * m.vx) else m.x + t * vx'
        y' = min (max (floorLevel s.world m) (m.y + t * vy')) ((ceilingLevel s.world m)-24)

        vy'' = if (m.y == y') then 0 else vy'
    in  { m | x <- x', y <- max 0 y', vy <- vy'', vx <- vx', jumpEnergy <- je'}

-- true if two denizens overlap
intersectDen : Denizen -> Denizen -> Bool
intersectDen a b = False
