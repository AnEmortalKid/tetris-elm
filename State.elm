module State where

import Basics exposing (..)
import Board exposing (Board)
import Controller exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Signal
import Tetromino exposing (Tetromino)
import Time exposing (Time)
import Random exposing (Generator, Seed)


{- A Record describing the state of our Game -}
type alias State = { falling : Tetromino
                   , seed : Seed
                   , bag : List Tetromino
                   , board : Board
                   , time : Time
                   , nextShift : Time
                   , shiftDelay : Time
                   }

startingShift : (Int, Int)
startingShift = (20, 5)
                   
                   
initialSeed = 42
                   
{- Our default starting state -}
defaultState : State
defaultState = 
    let (bag, seed) = Random.generate Tetromino.bag (Random.initialSeed initialSeed)
        falling = List.head bag |>
                  Maybe.withDefault Tetromino.i
        bag' = List.drop 1 bag
    in { falling = Tetromino.shift startingShift falling 
                , seed = seed
                , bag = bag'
                , board = Board.new []
                , time = 0 
                , nextShift = Time.second
                , shiftDelay = Time.second
               }

{- Converts a state to an element that can be rendered on the screen -}
view : State -> Element
view state =
    let screenWidth = 800
        screenHeight = 800
        boardForm = Board.addTetromino state.falling state.board |> Board.toForm
    in collage screenWidth screenHeight [ boardForm ]  
    
checkTick : State -> State
checkTick state = 
    if ( state.time < state.nextShift ) then state
    else 
        let shifted = Tetromino.shift (-1, 0) state.falling
            nextShift = state.time + state.shiftDelay
            isValid = Board.isValid shifted state.board
            state' = 
                if isValid then { state | falling = shifted }
                else nextTetromino state
        in { state' | nextShift = nextShift }

checkBag : State -> State
checkBag state = 
            if not (List.isEmpty state.bag) then state
            else let (bag,seed) = Random.generate Tetromino.bag state.seed
                in { state | bag = bag
                            , seed = seed
                    }
                    
nextTetromino : State -> State
nextTetromino state = 
    let state' = checkBag state
        nextFalling = List.head state'.bag |>
                      Maybe.withDefault Tetromino.i |>
                      Tetromino.shift startingShift
        nextBag = List.drop 1 state'.bag
        (lines, nextBoard) = Board.addTetromino state'.falling state'.board |>
                    Board.clearLines
    in { state' | 
        falling = nextFalling, 
        bag = nextBag, 
        board = nextBoard
    }
         
{- Try kicking tetromino into the next state -}
tryKicks : List (Int, Int) -> State -> State -> State
tryKicks shifts current nextState = 
    case shifts of
        [] -> current
        (s :: rest) -> 
            let shifted = Tetromino.shift s nextState.falling
            in  if Board.isValid shifted nextState.board then { nextState | falling = shifted }
                else tryKicks rest current nextState

{- either next state, or current state -}                
wallKick : State -> State -> State
wallKick current nextState = 
    let range = nextState.falling.cols // 2
        shifts = [1 .. range] |> List.concatMap (\n -> [(0, n), (0, -n)])
    in tryKicks shifts current nextState
         
floorKick : State -> State -> State
floorKick current nextState =
    let range = nextState.falling.rows // 2
        shifts = [1 .. range] |> List.map (\n -> (0,n))
    in tryKicks shifts current nextState
         
useIfValid : State -> State -> State
useIfValid current newState = 
    if Board.isValid newState.falling newState.board then newState
    else current
    
{- Applies the input to the State to produce the next state -}    
update : Input -> State -> State
update input state =
    let useIfValid' = useIfValid state
    in case input of
            Rotate -> 
                let rotated = { state | falling = Tetromino.rotate state.falling }
                    nextState = useIfValid' rotated
                    nextState' = 
                        if nextState == state then wallKick state rotated else nextState
                    nextState'' =
                                if nextState' == state then floorKick state rotated else nextState'
                in nextState''
            Shift amount -> useIfValid' { state | falling = Tetromino.shift amount state.falling }
            Tick delta -> useIfValid' <| checkTick { state | time = state.time + delta }
        
{- A signal of states for our game -}        
states : Signal State
states = Signal.foldp update defaultState inputs

main : Signal Element
main = Signal.map view states