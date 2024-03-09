{-# LANGUAGE ParallelListComp #-}

module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List as L
import Data.Fixed
import Data.HashSet as HS
import Graphics.Gloss.Interface.Pure.Game

--ячейка на доске
type Cell = (Int, Int)
--Любые ячейки, присутствующие в HashSet, живы, а все остальные мертвы.
type Board = HashSet Cell

--один шаг обновления состояния игровой доски
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


--обновления игрового состояния на основе прошедшего времени (deltaTime).
iterateGame :: Float -> Game -> Game
iterateGame deltaTime game = game { board  = if not $ paused game then iterateBoard (board game) else board game
                                  , camera = gameCamera { x    = x gameCamera + scaledDeltaX
                                                        , y    = y gameCamera + scaledDeltaY
                                                        , zoom = min zoomMaximum $ max nextZoom zoomMinimum}}
    where gameCamera :: Camera
          gameCamera = camera game

          nextZoom :: Float
          nextZoom = zoom gameCamera + scaledDeltaZoom
          zoomMinimum :: Float
          zoomMinimum = 0.05
          zoomMaximum :: Float
          zoomMaximum = 10.0

          scaledDeltaX :: Float
          scaledDeltaX = deltaX gameCamera / zoom gameCamera * deltaTime
          scaledDeltaY :: Float
          scaledDeltaY = deltaY gameCamera / zoom gameCamera * deltaTime
          scaledDeltaZoom :: Float
          scaledDeltaZoom = deltaZoom gameCamera * zoom gameCamera * deltaTime

-- визуализация игры
data Camera = Camera { x :: Float,    deltaX :: Float
                     , y :: Float,    deltaY :: Float
                     , zoom :: Float, deltaZoom :: Float}

-- отрисовка ячеек
drawCells :: Game -> Picture
drawCells (Game {board = board, camera = camera}) =
    scale (zoom camera) (zoom camera) $ translate (-x camera) (-y camera) $
    scale cellSize cellSize $ pictures $
        zipWith (uncurry translate) (L.map floatize $ HS.toList board)
                                    (replicate (HS.size board) (translate (-0.5) (-0.5) $ rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)

-- отрисовка сетки
drawGrid :: Game -> Picture
drawGrid (Game {camera = camera, screenSize = screenSize}) = pictures
    [ translate (-halfHorizontal) horizontalCameraOffset $ pictures
        [translate 0.0 offset line | line <- replicate horizontalLineCount $ Line [ (0.0, 0.0)
                                                                                  , (horizontalLineSize, 0.0)]
                                   | offset <- horizontalLineOffsets]
    , translate verticalCameraOffset (-halfVertical) $ pictures
        [translate offset 0.0 line | line <- replicate verticalLineCount $ Line [ (0.0, 0.0)
                                                                                , (0.0, verticalLineSize)]
                                   | offset <- verticalLineOffsets]]
    where horizontalLineSize :: Float
          horizontalLineSize = fromIntegral (fst screenSize)
          halfHorizontal :: Float
          halfHorizontal = horizontalLineSize / 2.0
          verticalLineSize :: Float
          verticalLineSize = fromIntegral (snd screenSize)
          halfVertical :: Float
          halfVertical = verticalLineSize / 2.0

          horizontalLineCount :: Int
          horizontalLineCount = length horizontalLineOffsets
          verticalLineCount :: Int
          verticalLineCount = length verticalLineOffsets

          horizontalLineOffsets :: [Float]
          horizontalLineOffsets = [-scaledCellSize, -scaledCellSize * 2.0 .. -halfVertical] ++ 0 : [scaledCellSize, scaledCellSize * 2.0 .. halfVertical + scaledCellSize]
          verticalLineOffsets :: [Float]
          verticalLineOffsets = [-scaledCellSize, -scaledCellSize * 2.0 .. -halfHorizontal] ++ 0 : [scaledCellSize, scaledCellSize * 2.0 .. halfHorizontal + scaledCellSize]
          scaledCellSize :: Float
          scaledCellSize = cellSize * zoom camera

          horizontalCameraOffset :: Float
          horizontalCameraOffset = -(y camera `mod'` cellSize) * zoom camera
          verticalCameraOffset :: Float
          verticalCameraOffset = -(x camera `mod'` cellSize) * zoom camera

-- отрисовка экземпляра игры
drawGame :: Game -> Picture
drawGame game =
    pictures [ color black $ drawCells game
                , if showGrid game then color black $ drawGrid game else Blank
                , translate (-halfWidth + 10) (halfHeight - 40) $ scale 0.25 0.25 $ color red  $ text gameStateText
                ]
    where gameStateText = case paused game of
                             False -> ""
                             True  -> "Pause"
          (width, height) = fromIntegral <$> screenSize game
          halfWidth = fromIntegral width / 2.0
          halfHeight = fromIntegral height / 2.0



-- обрабатка пользовательских событий
gameInteract :: Event -> Game -> Game
gameInteract (EventKey (Char key) keyState _ _) game =
    case key of
         'w' -> game {camera = gameCamera {deltaY    = if keyState == Down then  moveSpeed else 0.0}}
         's' -> game {camera = gameCamera {deltaY    = if keyState == Down then -moveSpeed else 0.0}}
         'a' -> game {camera = gameCamera {deltaX    = if keyState == Down then -moveSpeed else 0.0}}
         'd' -> game {camera = gameCamera {deltaX    = if keyState == Down then  moveSpeed else 0.0}}
         '+' -> game {camera = gameCamera {deltaZoom = if keyState == Down then  zoomSpeed else 0.0}}
         '-' -> game {camera = gameCamera {deltaZoom = if keyState == Down then -zoomSpeed else 0.0}}
         'g' -> if keyState == Down then game {showGrid = not $ showGrid game} else game
         'r' -> if keyState == Down then game {board = HS.empty, paused = True} else game
         _   -> game
    where gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventKey (SpecialKey key) keyState _ _) game =
    case key of
         KeyUp       -> game {camera = gameCamera {deltaY    = if keyState == Down then  moveSpeed else 0.0}}
         KeyDown     -> game {camera = gameCamera {deltaY    = if keyState == Down then -moveSpeed else 0.0}}
         KeyLeft     -> game {camera = gameCamera {deltaX    = if keyState == Down then -moveSpeed else 0.0}}
         KeyRight    -> game {camera = gameCamera {deltaX    = if keyState == Down then  moveSpeed else 0.0}}
         KeyPageUp   -> game {camera = gameCamera {deltaZoom = if keyState == Down then  zoomSpeed else 0.0}}
         KeyPageDown -> game {camera = gameCamera {deltaZoom = if keyState == Down then -zoomSpeed else 0.0}}
         KeySpace    -> if keyState == Down then game {paused = not $ paused game} else game
         _           -> game
    where gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventKey (MouseButton mouseButton) Down _ position) game =
    case mouseButton of
         LeftButton -> if not $ mouseToCellCoordinates `HS.member` board game
             then game {board = HS.insert mouseToCellCoordinates (board game)}
             else game {board = HS.delete mouseToCellCoordinates (board game)}
         _          -> game
    where mouseToCellCoordinates :: Cell
          mouseToCellCoordinates =
              ( floor $ (fst position / zoom gameCamera + x gameCamera) / cellSize + 1.0
              , floor $ (snd position / zoom gameCamera + y gameCamera) / cellSize + 1.0)

          gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventResize newScreenSize) game = game {screenSize = newScreenSize}
gameInteract _ game = game


-- игровое состояние
data Game = Game { board      :: Board
                 , camera     :: Camera
                 , paused     :: Bool
                 , showGrid   :: Bool
                 , screenSize :: (Int, Int)}

-- новая игра
newGame :: Game
newGame = Game { board  = HS.empty
               , camera = Camera { x = 0.0,    deltaX = 0.0
                                 , y = 0.0,    deltaY = 0.0
                                 , zoom = 3.0, deltaZoom = 0.0}
               , paused = True
               , showGrid = True
               , screenSize = defaultScreenSize}


title :: String
title = "Conway's game of life"

defaultScreenSize :: (Int, Int)
defaultScreenSize = (800, 600)

backgroundColor :: Color
backgroundColor = white

iterationsPerSecond :: Int
iterationsPerSecond = 10

moveSpeed :: Float
moveSpeed = 120.0

zoomSpeed :: Float
zoomSpeed = 1.0

cellSize :: Float
cellSize = 10.0

main :: IO ()
main = play (InWindow title defaultScreenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame