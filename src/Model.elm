--
-- Model for an Asteroids-like video game
--
module Model where

import Time exposing (Time)
import List


type alias Object = {
    x : Float,
    y : Float,
    vx : Float,
    vy : Float,
    ang : Float,
    angV : Float    
  }

type alias Laser 
  = (Object, Time)  -- time before decay

type alias Asteroid
  = (Object, Float)  -- rock size
              

type alias Model = {
    ship : Object,
    lasers : List Laser,
    asteroids : List Asteroid,
    score : Int
  }

       

-- | advance time on all objects
advance : Time -> Model -> Model
advance dt model = 
  { model |
      ship = move1 dt model.ship,
      asteroids = List.map (\(obj,size) -> (move1 dt obj,size)) model.asteroids,
      lasers= List.map (\(obj,time) -> (move1 dt obj,time)) model.lasers
      }

-- | advance time on a single object 
move1 : Time -> Object -> Object
move1 dt obj 
  = let
    wrap h m = if h>m then h-2*m else if h < -m then h+2*m else h
    x' = wrap (obj.x + dt*obj.vx) maxX
    y' = wrap (obj.y + dt*obj.vy) maxY
    ang' = obj.ang + dt*obj.angV
 in { obj | x=x', y=y', ang=ang' }


-- | laser decay 
decay : Time -> Model -> Model
decay dt model 
  = { model | lasers = 
        List.filter (\(obj,t) -> t>0) <| 
        List.map (\(obj,t) -> (obj,t-dt)) model.lasers }




-- | collision detection
-- laser and asteroids only (for simplicity)
collisions : Model -> Model
collisions model = 
  let 
    -- splits asteroids that have been hit by lasers
    (rocks, rocks') 
      = List.partition 
        (\rock -> List.any (collision rock) model.lasers) model.asteroids
    -- lasers that have not hit any rocks
    lasers' = List.filter 
              (\laser -> not (List.any (flip collision laser) rocks)) model.lasers
  in { model | 
         asteroids = List.concatMap fragment rocks ++ rocks',
         lasers = lasers',
         score = model.score + 50*List.length rocks
     }



-- fragment an asteroid
fragment : Asteroid -> List Asteroid
fragment (obj, size) = 
  if size>=1 then 
    let size' = 0.5*size
    in [ ({ obj | vx=obj.vy, vy=obj.vx }, size'),
         ({ obj | vx=-obj.vx, vy=obj.vy}, size'),
         ({ obj | vx=obj.vx, vy=-obj.vy}, size'),
         ({ obj | vx=-obj.vx, vy=-obj.vy}, size') ]
  else 
    []


-- check a single collision
collision : Asteroid -> Laser -> Bool
collision (obj1,size) (obj2,time) = sqDistance obj1 obj2 <= 100*size^2


-- | square of euclidean distance
sqDistance : Object -> Object  -> Float
sqDistance obj1 obj2 = (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2


-- | fire a laser
fireLaser : Model -> Model
fireLaser model = 
  let ship = model.ship
      newLaser = {   x=ship.x, y=ship.y,
                     vx= 400*cos (ship.ang), 
                     vy= 400*sin (ship.ang), 
                     ang=ship.ang, angV = 0 
                 }
      fireDelay = 1
  in { model | lasers = (newLaser, fireDelay) :: model.lasers }


-- | control the ship
moveShip : { x:Int, y:Int } -> Model -> Model
moveShip arrows model =
   let ship = model.ship
       ship' = { ship | angV = negate (toFloat arrows.x)*pi,
                   vx = 50*(cos ship.ang)*(toFloat arrows.y),
                   vy = 50*(sin ship.ang)*(toFloat arrows.y)
               }
   in { model | ship = ship' }


    
---
--- constants
---
maxX : Float
maxX = 300

maxY : Float
maxY = 300


--
-- initial model
--
initial : Model
initial = 
  { ship = { x = 0, y = 0, vx= 0, vy= 0, ang=0, angV = 0 }
  , asteroids = [ ({x=-200,y=-200, vx=10, vy=10, ang=0, angV=pi}, 2),
                  ({x= 100,y= 100, vx=20, vy=-20, ang=0, angV=-0.5*pi}, 4),
                  ({x= 100, y=-200, vx=-10, vy=-10, ang=pi, angV=pi}, 2.5),
                  ({x= 0, y=-200, vx=-10, vy=-10, ang=pi, angV=pi}, 2)
                ]
  , lasers = []
  , score = 0
  }

