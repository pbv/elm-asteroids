--
-- Sample Asteroids-like video game in Elm
-- Pedro Vasconcelos, 2015
--
-- Main module: signals setup and updates
-- 
module Asteroids where

import Model exposing (Model)
import View 

import Signal
import Time exposing (Time)
import Graphics.Element exposing (Element, show)
import Keyboard
import Window
import Touch exposing (Touch)

type alias Vec = 
  { x: Int, y : Int }

type Update = TimeDelta Time
            | Movement Vec
            | Fire


-- | react to an update
update : Update -> Model -> Model
update updt model 
  = case updt of
      TimeDelta dt -> model |>
                      Model.advance dt |> 
                      Model.decay dt |> 
                      Model.collisions 
      Movement vect -> 
        Model.moveShip vect model 
      Fire -> 
        Model.fireLaser model


--- putting it all together
main : Signal Element
main = Signal.map2 View.view Window.dimensions game

game : Signal Model
game = Signal.foldp update Model.initial <|
       Signal.merge timeUpdates controlUpdates 

--
-- individual update signals
--
-- | timed updates
timeUpdates = Signal.merge autofireUpdates
              (Signal.map (\t -> TimeDelta (t/Time.second)) (Time.fps 60))
                

autofireUpdates = Signal.map (\_ -> Fire) (Time.every Time.second)

-- | user-controlled updates
controlUpdates = Signal.merge keyboardUpdates touchUpdates

keyboardUpdates = 
  Signal.merge 
  (Signal.map Movement Keyboard.arrows)
  (Signal.map (\_ -> Fire) <| Signal.filter identity False Keyboard.space)
                  

touchUpdates = 
  let decode (w, h) ts
      = case ts of 
        [] -> Movement { x=0, y=0 }
        t :: _ -> 
          if t.y < h//2 then 
            if t.x < w//2 then 
              Movement { x=0, y=1 }
            else
              Fire
          else
            Movement { x = if t.x < w//2 then -1 else 1,
                       y = 0 }

  in
    Signal.map2 decode Window.dimensions Touch.touches








