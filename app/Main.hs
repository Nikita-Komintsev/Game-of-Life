{-# LANGUAGE ParallelListComp #-}

module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List as L
import Data.Fixed
import Data.HashSet as HS


type Cell = (Int, Int)

type Board = HashSet Cell

iterateBoard :: Board -> Board
iterateBoard board = stepCells (HS.toList board) HS.empty
    where stepCells :: [Cell] -> Board -> Board
          stepCells [] accumulator          = accumulator
          stepCells (cell:rest) accumulator = if aliveNeighbors >= 2 && aliveNeighbors < 4
              then stepCells rest (HS.insert cell tryAddOffspring)
              else stepCells rest tryAddOffspring
              where aliveNeighbors :: Int
                    aliveNeighbors = length $ findAlive possibleNeighbors
                    findAlive :: [Cell] -> [Cell]
                    findAlive = L.filter (`HS.member` board)

                    tryAddOffspring :: Board
                    tryAddOffspring
                      | not $ L.null findOffspring = HS.union findOffspring accumulator
                      | otherwise                  = accumulator

                    findOffspring :: Board
                    findOffspring = HS.fromList [deadNeighbor | deadNeighbor <- possibleNeighbors
                                                              , not $ deadNeighbor `HS.member` board
                                                              , not $ deadNeighbor `HS.member` accumulator
                                                              , length (findAlive $ findPossibleNeighbors deadNeighbor) == 3]

                    possibleNeighbors :: [Cell]
                    possibleNeighbors = findPossibleNeighbors cell
                    findPossibleNeighbors :: Cell -> [Cell]
                    findPossibleNeighbors cell = [(fst cell + dx, snd cell + dy) | dx <- [-1, 0, 1]
                                                                                 , dy <- [-1, 0, 1]
                                                                                 , (dx, dy) /= (0, 0)]


iterateGame :: Float -> Game -> Game
iterateGame deltaTime game = game { board  = if not $ paused game then iterateBoard (board game) else board game}



drawCells :: Game -> Picture
drawCells (Game {board = board}) =
    scale cellSize cellSize $ pictures $
        zipWith (uncurry translate) (L.map floatize $ HS.toList board)
                                    (replicate (HS.size board) (translate (-0.5) (-0.5) $ rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)


drawGrid :: Game -> Picture
drawGrid (Game {screenSize = screenSize}) = pictures
    [ translate (-halfHorizontal) 10 $ pictures
        [translate 0.0 offset line | line <- replicate 10 $ Line [ (0.0, 0.0)
                                                                                  , (horizontalLineSize, 0.0)]
                                   | offset <- [1.0]]
    , translate 10 (-halfVertical) $ pictures
        [translate offset 0.0 line | line <- replicate 10 $ Line [ (0.0, 0.0)
                                                                                , (0.0, verticalLineSize)]
                                   | offset <- [1.0]]]
    where horizontalLineSize :: Float
          horizontalLineSize = fromIntegral (fst screenSize)
          halfHorizontal :: Float
          halfHorizontal = horizontalLineSize / 2.0
          verticalLineSize :: Float
          verticalLineSize = fromIntegral (snd screenSize)
          halfVertical :: Float
          halfVertical = verticalLineSize / 2.0


drawGame :: Game -> Picture
drawGame game = pictures [ color black $ drawCells game
                        ]


gameInteract :: Event -> Game -> Game
gameInteract (EventKey (SpecialKey key) keyState _ _) game =
    case key of
         KeySpace    -> if keyState == Down then game {paused = not $ paused game} else game
         _           -> game

gameInteract (EventKey (MouseButton mouseButton) Down _ position) game =
    case mouseButton of
         LeftButton -> if not $ mouseToCellCoordinates `HS.member` board game
             then game {board = HS.insert mouseToCellCoordinates (board game)}
             else game {board = HS.delete mouseToCellCoordinates (board game)}
         _          -> game
    where mouseToCellCoordinates :: Cell
          mouseToCellCoordinates =
              ( floor $ (fst position) / cellSize + 1.0
              , floor $ (snd position ) / cellSize + 1.0)

gameInteract (EventResize newScreenSize) game = game {screenSize = newScreenSize}
gameInteract _ game = game



data Game = Game { board      :: Board
                 , paused     :: Bool
                 , screenSize :: (Int, Int)}

newGame :: Game
newGame = Game { board  = HS.empty
               , paused = True
               , screenSize = defaultScreenSize}


title :: String
title = "Conway's game of life"

defaultScreenSize :: (Int, Int)
defaultScreenSize = (600, 600)

backgroundColor :: Color
backgroundColor = white

iterationsPerSecond :: Int
iterationsPerSecond = 10


cellSize :: Float
cellSize = 10.0

main :: IO ()
main = play (InWindow title defaultScreenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame
