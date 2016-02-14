module State where

import Controller exposing (..)
import Basics exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Tetromino exposing (Tetromino)
import Time exposing (Time)

{- A Record describing the state of our Game -}
type alias State = { falling : Tetromino
                   , time : Time
                   , nextShift : Time
                   , shiftDelay : Time
                   }

{- Our default starting state -}
defaultState : State
defaultState = { falling = Tetromino.j 
                , time = 0 
                , nextShift = Time.second
                , shiftDelay = Time.second
               }

{- Converts a state to an element that can be rendered on the screen -}
view : State -> Element
view state =
    let screenWidth = 800
        screenHeight = 800
        fallingForm = Tetromino.toForm state.falling
    in collage screenWidth screenHeight [ fallingForm ]  
    
checkTick : State -> State
checkTick state = 
    if ( state.time < state.nextShift ) then state
    else { state | falling = Tetromino.shift (-1, 0) state.falling
                   , nextShift = state.time + state.shiftDelay
         }
    
{- Applies the input to the State to produce the next state -}    
update : Input -> State -> State
update input state = 
    case  input of
        Rotate ->  { state | falling = Tetromino.rotate state.falling }
        Shift amount -> { state | falling = Tetromino.shift amount state.falling }
        Tick delta -> checkTick { state | time = state.time + delta }
        
{- A signal of states for our game -}        
states : Signal State
states = Signal.foldp update defaultState inputs

main : Signal Element
main = Signal.map view states