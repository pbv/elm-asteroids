

module Asteroids where

import Model exposing (Model)
import View 

import Signal
import Time exposing (Time)
import Graphics.Element exposing (Element, show)
import Keyboard
import Random

import Debug

type alias Keys = 
  { x: Int, y : Int }

type Update = TimeDelta Time
            | MoveShip Keys
            | FireLaser


update : Update -> Model -> Model
update updt model 
  = case updt of
      TimeDelta dt -> model |>
                      Model.move dt |> 
                      Model.decay dt |> 
                      Model.collisions 
      MoveShip arrows -> 
        let ship = model.ship
            ship' = { ship | angV = negate (toFloat arrows.x)*pi,
                        vx = 50*(cos ship.ang)*(toFloat arrows.y),
                        vy = 50*(sin ship.ang)*(toFloat arrows.y)
                    }
        in { model | ship = ship' }
      FireLaser -> 
        let ship = model.ship
            newLaser = {   x=ship.x, y=ship.y,
                           vx= 400*cos (ship.ang), 
                           vy= 400*sin (ship.ang), 
                             ang=ship.ang, angV = 0 }
            delay = 1
        in { model | lasers = (newLaser,delay) :: model.lasers }




--- putting it all together
main : Signal Element
main = Signal.map View.view game


game : Signal Model
game = Signal.foldp update Model.initial <|
       Signal.merge keyboardUpdates timeUpdates

--
-- individual update signals
--
timeUpdates = Signal.map (\t -> TimeDelta (t/1000)) (Time.fps 60)

keyboardUpdates = Signal.merge fireKey moveKeys

moveKeys = Signal.map MoveShip Keyboard.arrows

fireKey = Signal.map (\_ -> FireLaser) (Signal.filter identity True Keyboard.space)

