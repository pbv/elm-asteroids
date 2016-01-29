--
-- Sample Asteroids-like video game in Elm
-- Pedro Vasconcelos, 2015
--
-- View module: rendering the model as a graphic element
-- 
module View where

import Model exposing (Model)

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color
import Text

-- | rendering with given width and height
view : (Int,Int) -> Model -> Element
view (width,height) model =       
  color Color.black <|
  collage width height <|
    [ showScore (height//2) model.score
    , drawShip model.ship ] ++
    List.map drawAsteroid model.asteroids ++
    List.map drawLaser model.lasers 


showScore height score = 
  moveY (toFloat height - 20) <|
  text <| 
  Text.color Color.white <| 
  Text.monospace <|
  Text.height 30 <|
  Text.fromString <| 
  toString score

drawAsteroid (obj,size) = draw obj (scale size asteroid)
drawLaser (obj,time) = draw obj laser

drawShip obj =
  if obj.vx^2 + obj.vy^2 > 0  then 
    draw obj (group [thrust |> move (-10,0), ship])
  else
    draw obj ship

draw obj shape = 
  shape |> 
  rotate obj.ang |> 
  move (obj.x, obj.y) 

-- | basic shapes         
ship = polygon [(15,0),(-10,10), (-5,0),(-10,-10),(15,0)] |>
       filled Color.green

thrust = oval 20 5 |> filled Color.white
         
asteroid = 
  let 
    shape = polygon [(-10,5),(-5, 10),(10,5), (5,-5),(-5,-10),(-10,5)] 
  in 
    group [ shape |> filled Color.red
          , shape |> outlined { defaultLine | color=Color.white, width=1 }
          ]

laser = 
  segment (0,0) (20,0) |> 
  traced { defaultLine | color=Color.yellow, width=4 }

-------------------------
-- testing code
-------------------------

main = view (600,600) Model.initial


