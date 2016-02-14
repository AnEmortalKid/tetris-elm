module GameBoard where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (Color)

type alias GameBoard = { borderColor : Color }

screenWidth : Float
screenWidth = 400

screenHeight : Float
screenHeight = 400

toElement : GameBoard -> Element
toElement gameboard =
    let shape = rect screenWidth screenHeight
        border = outlined (solid gameboard.borderColor ) shape
    in collage (round screenWidth) (round screenHeight) [border]
    
main : Element
main = toElement (GameBoard Color.black)