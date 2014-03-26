import Keyboard
import Window
import Dict
import Physics (..)

-- Game entities
type Mario = Denizen
type Controls = (Float, IVec)
type Time = Float
type Enemy = Denizen  
{- Elm doesn't allow us to recursively define functions inside dependent types, so we have to fake it with Denizen & World, rather than scene and Enemy as we'd like -}

defaultWorld = Dict.fromList [
    (-8,[4]),(-6,[1,2,3,4,5,6])
    ,(-3,[4]),(1,[10,3,1]),(2,[9,2])
    ,(3,[8,3]),(4,[4]),(5,[5,1])
    ,(6,[6,1]),(7,[6,1]),(8,[5,1])
    ]


-- Structural classes
type Positional a = {a | x:Float, y:Float}

-- MODEL
defaultMario = { x=0, y=0, vx=0, vy=0, dir=1, jumpEnergy=maxJump, w=35, h=35}

goombaAI : World -> Denizen -> Denizen
goombaAI w e = if (blockedX w {e|x <- e.x+e.dir}) then {e| dir <- (-e.dir), vx <- (-e.dir / 2)} else {e|vx<-(e.dir/2)}

defaultGoomba = {x=-16,y=0,vx=-0.5, vy=0, dir=-1, jumpEnergy=0, w=16, h=16}

scene_world_1_1 = {world = defaultWorld, mario = defaultMario, enemies = [defaultGoomba]}
{- To do:
 - enemies and death
 - squishing enemies
 - bumping blocks from under-side
 - going down pipes
 - scenes, file loading?
 -}

-- apply walking (side-to-side) control, set direction for graphics
walk : IVec -> Mario -> Mario
walk {x} m = { m | vx <- toFloat x, dir <- if (x ==0) then m.dir else toFloat x}

updateDenizen : Time -> Scene -> Denizen -> Denizen
updateDenizen t scene = physics t scene . gravity t scene

updateGoomba : Time -> Scene -> Denizen -> Maybe Denizen
updateGoomba t s a = 
    let newG = (goombaAI s.world . updateDenizen t s) a
        imp = intersectDen s.mario newG
    in case imp of
        None -> Just newG
        Stomp -> Nothing
        Hit -> Just newG

-- Apply all the things!
step : Controls -> Scene -> Scene
step (t,dir) scene = 
    let controlMario = walk dir . jump dir scene
    in {scene | mario <- updateDenizen t scene (controlMario scene.mario), enemies <- justs (map (updateGoomba t scene) scene.enemies)}


-- DISPLAY
render : (Int, Int) -> Scene -> Element 
render (w',h') scene =
    let (w,h) = (toFloat w', toFloat h')
        mario = scene.mario
        verb = if | not (onSurface scene.world mario) -> "jump"
                  | mario.vx /= 0 -> "walk"
                  | otherwise     -> "stand"
        dir = if (mario.dir > 0) then "right" else "left"
        src  = "/imgs/mario/" ++ verb ++ "-" ++ dir ++ ".gif"
    in 
--        (asText mario) `above`
--        (asText scene.enemies) `above`
        collage w' h' (
            renderBackground (w,h) scene.world ++
            [toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)] ++
            renderBlocks (w,h) scene.world ++
            renderEnemies (w,h) scene.enemies
            )

renderEnemies : (Float, Float) -> [Denizen] -> [Form]
renderEnemies (w,h) es =
    let r1 e = toForm (image 16 16 "/imgs/enemies/goomba.gif") |> move (e.x, e.y + 56 - h/2)
    in  map (r1) es

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

