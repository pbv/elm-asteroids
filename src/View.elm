
module View where


import Model exposing (Model)

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color
import Text

view : Model -> Element
view model =       
  color Color.black <|
  collage (round (2*Model.maxX)) (round (2*Model.maxY)) <| 
  drawScore model.score ::
  drawShip model.ship ::
  List.map drawAsteroid model.asteroids ++
  List.map drawLaser model.lasers


drawScore score = 
  moveY (Model.maxY-20) <|
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
              

ship = polygon [(15,0),(-10,10), (-5,0),(-10,-10),(15,0)] |>
       filled Color.green

thrust = oval 20 5 |> filled Color.white
         



asteroid = 
  let fig = polygon [(-10,5),(-5, 10),(10,5), (5,-5),(-5,-10),(-10,5)] 
  in group [ fig |> filled Color.red
           , fig |> outlined { defaultLine | color=Color.white, width=1 }
           ]

laser = segment (0,0) (10,0) |> 
        traced { defaultLine | color=Color.yellow, width=4 }



-------------------------
-- testing code
-------------------------

main = view Model.initial


