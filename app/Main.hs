{-# LANGUAGE ParallelListComp #-}

module Main (main) where
import Data.List as L
import Data.Fixed
import Data.HashSet as HS
import Graphics.Gloss.Interface.Pure.Game

--ячейка на доске
type Cell = (Int, Int)
--Любые ячейки, присутствующие в HashSet, живы, а все остальные мертвы.
type Board = HashSet Cell

data Configuration = Glider
                  | GliderGunSE
                  | GliderGunSW
                  | GliderGunNE
                  | GliderGunNW
                  | NOTGate
                  | ANDGate
                  | ORGate

initialBoard :: Configuration -> Board
initialBoard Glider = HS.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (1, 2)]
initialBoard GliderGunSE = HS.fromList [(12,-3), (13,-3), (11,-2), (15,-2), (10,-1), (16,-1), (24,-1), (0,0), (1,0), (10,0), (14,0), (16,0), (17,0), (22,0), (24,0), (0,1), (1,1), (10,1), (16,1), (20,1), (21,1), (11,2), (15,2), (20,2), (21,2), (34,2), (35,2), (12,3), (13,3), (20,3), (21,3), (34,3), (35,3), (22,4), (24,4), (24,5)]
initialBoard GliderGunSW = HS.fromList [(-12,-3), (-13,-3), (-11,-2), (-15,-2), (-10,-1), (-16,-1), (-24,-1), (0,0), (-1,0), (-10,0), (-14,0), (-16,0), (-17,0), (-22,0), (-24,0), (0,1), (-1,1), (-10,1), (-16,1), (-20,1), (-21,1), (-11,2), (-15,2), (-20,2), (-21,2), (-34,2), (-35,2), (-12,3), (-13,3), (-20,3), (-21,3), (-34,3), (-35,3), (-22,4), (-24,4), (-24,5)]
initialBoard GliderGunNE = HS.fromList [(0, 0), (1, 0), (0, 1), (1, 1), (0, 10), (1, 10), (2, 10), (-1, 11), (3, 11), (-2, 12), (4, 12), (-2, 13), (4, 13), (1, 14), (-1, 15), (3, 15), (0, 16), (1, 16), (2, 16), (1, 17), (-2, 20), (-1, 20), (0, 20), (-2, 21), (-1, 21), (0, 21), (-3, 22), (1, 22), (-4, 24), (-3, 24), (1, 24), (2, 24), (-2, 34), (-1, 34), (-2, 35), (-1, 35)]
initialBoard GliderGunNW = HS.fromList [(0, 0), (-1, 0), (0, 1), (-1, 1), (0, 10), (-1, 10), (-2, 10), (1, 11), (-3, 11), (2, 12), (-4, 12), (2, 13), (-4, 13), (-1, 14), (1, 15), (-3, 15), (0, 16), (-1, 16), (-2, 16), (-1, 17), (2, 20), (1, 20), (0, 20), (2, 21), (1, 21), (0, 21), (3, 22), (-1, 22), (4, 24), (3, 24), (-1, 24), (-2, 24), (2, 34), (1, 34), (2, 35), (1, 35)]

initialBoard NOTGate = HS.fromList [(12,-3), (13,-3), (11,-2), (15,-2), (10,-1), (16,-1), (24,-1), (0,0), (1,0), (10,0), (14,0), (16,0), (17,0), (22,0), (24,0), (0,1), (1,1), (10,1), (16,1), (20,1), (21,1), (11,2), (15,2), (20,2), (21,2), (34,2), (35,2), (12,3), (13,3), (20,3), (21,3), (34,3), (35,3), (22,4), (24,4), (24,5),
                                    (15, -48),  (16, -48),  (15, -47),  (16, -47),  (15, -38),  (16, -38),  (17, -38),  (14, -37),  (18, -37),  (13, -36),  (19, -36),  (13, -35),  (19, -35),  (16, -34),  (14, -33),  (18, -33),  (15, -32),  (16, -32),  (17, -32),  (16, -31),  (13, -28),  (14, -28),  (15, -28),  (13, -27),  (14, -27),  (15, -27),  (12, -26),  (16, -26),  (11, -24),  (12, -24),  (16, -24),  (17, -24),  (13, -14),  (14, -14),  (13, -13),  (14, -13),
                                    (23, 43), (24, 43), (23, 44), (24, 44), (23, 53), (24, 53), (25, 53), (22, 54), (26, 54), (21, 55), (27, 55), (21, 56), (27, 56), (24, 57), (22, 58), (26, 58), (23, 59), (24, 59), (25, 59), (24, 60), (21, 63), (22, 63), (23, 63), (21, 64), (22, 64), (23, 64), (20, 65), (24, 65), (19, 67), (20, 67), (24, 67), (25, 67), (21, 77), (22, 77), (21, 78), (22, 78),
                                    (72, 269), (73, 269), (71, 270), (75, 270), (70, 271), (76, 271), (84, 271), (60, 272), (61, 272), (70, 272), (74, 272), (76, 272), (77, 272), (82, 272), (84, 272), (60, 273), (61, 273), (70, 273), (76, 273), (80, 273), (81, 273), (71, 274), (75, 274), (80, 274), (81, 274), (94, 274), (95, 274), (72, 275), (73, 275), (80, 275), (81, 275), (94, 275), (95, 275), (82, 276), (84, 276), (84, 277),
                                    (-83, 263), (-82, 263), (-83, 264), (-82, 264)]

initialBoard ANDGate = HS.fromList [(0,0),(0,1),(1,0),(1,1),
                                    (-1,14),(-1,15),(0,14),(0,15),
                                    (6,0),(7,0),(8,0),(8,-1),(5,1),(5,2),(6,2),
                                    (4,15),(3,16),(5,16),(4,17),(6,17),(6,18),(6,19),(7,19),
                                    (-65, 54), (-65, 55), (-64, 54), (-64, 55),
                                    (-44, -54), (-44, -53), (-43, -54), (-43, -53)
                                    ]

initialBoard ORGate = HS.fromList [(8,7),(8,8),(9,7),(9,8),
                                  (-66, -70), (-65, -70), (-66, -69), (-65, -69),
                                  (-58, 7), (-57, 7), (-59, 8), (-55, 8), (-60, 9), (-54, 9), (-46, 9),(-70, 10), (-69, 10), (-60, 10), (-56, 10), (-54, 10), (-53, 10), (-48, 10),(-46, 10), (-70, 11), (-69, 11), (-60, 11), (-54, 11), (-50, 11), (-49, 11),(-59, 12), (-55, 12), (-50, 12), (-49, 12), (-36, 12), (-35, 12), (-58, 13),(-57, 13), (-50, 13), (-49, 13), (-36, 13), (-35, 13), (-48, 14), (-46, 14),(-46, 15),
                                  (52,-4),(52,-3),(53,-4),(53,-3),
                                  (41, -7), (40, -7), (42, -6), (38, -6), (43, -5), (37, -5), (29, -5),(53, -4), (52, -4), (43, -4), (39, -4), (37, -4), (36, -4), (31, -4),(29, -4), (53, -3), (52, -3), (43, -3), (37, -3), (33, -3), (32, -3),(42, -2), (38, -2), (33, -2), (32, -2), (19, -2), (18, -2), (41, -1),(40, -1), (33, -1), (32, -1), (19, -1), (18, -1), (31, 0), (29, 0),(29, 1)
                                  ]


-- Тип данных для представления выбора конфигурации
data ConfigurationChoice = GliderChoice
                          | GliderGunSEChoice
                          | GliderGunSWChoice
                          | GliderGunNEChoice
                          | GliderGunNWChoice
                          | NOTGateChoice
                          | ANDGateChoice
                          | ORGateChoice

-- Функция для добавления выбранной конфигурации на игровое поле
addConfiguration :: ConfigurationChoice -> Cell -> Board -> Board
addConfiguration choice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoardForChoice choice

initialBoardForChoice :: ConfigurationChoice -> Board
initialBoardForChoice GliderChoice = initialBoard Glider
initialBoardForChoice GliderGunSEChoice = initialBoard GliderGunSE
initialBoardForChoice GliderGunSWChoice = initialBoard GliderGunSW
initialBoardForChoice GliderGunNEChoice = initialBoard GliderGunNE
initialBoardForChoice GliderGunNWChoice = initialBoard GliderGunNW
initialBoardForChoice NOTGateChoice = initialBoard NOTGate
initialBoardForChoice ANDGateChoice = initialBoard ANDGate
initialBoardForChoice ORGateChoice = initialBoard ORGate


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
                    findPossibleNeighbors cell = [(fst cell + dx, snd cell + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]
                                                 , (dx, dy) /= (0, 0)]


--обновления игрового состояния на основе прошедшего времени (deltaTime).
iterateGame :: Float -> Game -> Game
iterateGame deltaTime game = game { board  = if not $ paused game then iterateBoard (board game) else board game
                                  , camera = gameCamera { x    = x gameCamera + scaledDeltaX
                                                        , y    = y gameCamera + scaledDeltaY
                                                        , zoom = min zoomMaximum $ max nextZoom zoomMinimum}
                                  }
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
                     , zoom :: Float, deltaZoom :: Float
                     }

-- отрисовка ячеек
drawCells :: Game -> Picture
drawCells (Game {board = board, camera = camera}) =
    scale (zoom camera) (zoom camera) $ translate (-x camera) (-y camera) $
    scale cellSize cellSize $ pictures $
        zipWith (uncurry translate) (L.map floatize $ HS.toList board) (replicate (HS.size board) (translate (-0.5) (-0.5) $ rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)

-- отрисовка сетки
drawGrid :: Game -> Picture
drawGrid (Game {camera = camera, screenSize = screenSize}) = pictures
    [ translate (-halfHorizontal) horizontalCameraOffset $ pictures
        [translate 0.0 offset line | line <- replicate horizontalLineCount $ Line [ (0.0, 0.0), (horizontalLineSize, 0.0)]
                                   | offset <- horizontalLineOffsets
        ]
    , translate verticalCameraOffset (-halfVertical) $ pictures
        [translate offset 0.0 line | line <- replicate verticalLineCount $ Line [ (0.0, 0.0), (0.0, verticalLineSize)]
                                   | offset <- verticalLineOffsets
        ]
    ]
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

-- Тип данных для панели
data Panel = Panel { panelWidth :: Float, panelHeight :: Float }

-- Определение панели в новой игре
newPanel :: Panel
newPanel = Panel { panelWidth = fromIntegral $ fst defaultScreenSize, panelHeight = 150.0 }

-- Функция отрисовки панели
drawPanel :: Panel -> (Int, Int) -> Picture
drawPanel (Panel { panelWidth = width, panelHeight = height }) (screenWidth, screenHeight) =
    pictures [ translate 0 (-halfHeight + height / 2) $ color (greyN 0.9) $ rectangleSolid (fromIntegral screenWidth) height
             , translate (-halfWidth + 10 + 80 * 1) (-halfHeight + height / 2 + 40) $ button "Play"
             , translate (-halfWidth + 10 + 80 * 2) (-halfHeight + height / 2 + 40) $ button "Grid"
             , translate (-halfWidth + 10 + 80 * 3) (-halfHeight + height / 2 + 40) $ button "Reset"
             , translate (-halfWidth + 10 + 80 * 4) (-halfHeight + height / 2 + 40) $ button "NOT Gate"
             , translate (-halfWidth + 10 + 80 * 5) (-halfHeight + height / 2 + 40) $ button "AND Gate"
             , translate (-halfWidth + 10 + 80 * 6) (-halfHeight + height / 2 + 40) $ button "OR Gate"
             , translate (-halfWidth + 10 + 80 * 7) (-halfHeight + height / 2 + 40) $ scale 0.1 0.1 $ color black $ Text "WASD - Move camera"
             , translate (-halfWidth + 10 + 80 * 7) (-halfHeight + height / 2 + 20) $ scale 0.1 0.1 $ color black $ Text "(+)/(-) Zoom in/out"
             , translate (-halfWidth + 10 + 80 * 7) (-halfHeight + height / 2) $ scale 0.1 0.1 $ color black $ Text "(g) - Grid visibility"
             , translate (-halfWidth + 10 + 80 * 7) (-halfHeight + height / 2 - 20) $ scale 0.1 0.1 $ color black $ Text "(r) - Reset board"
             , translate (-halfWidth + 10 + 80 * 10) (-halfHeight + height / 2 + 40) $ scale 0.1 0.1 $ color black $ Text "(1) - Select Glider"
             , translate (-halfWidth + 10 + 80 * 10) (-halfHeight + height / 2 + 20) $ scale 0.1 0.1 $ color black $ Text "(2) - Select GliderGun SE"
             , translate (-halfWidth + 10 + 80 * 10) (-halfHeight + height / 2 ) $ scale 0.1 0.1 $ color black $ Text "(3) - Select GliderGun NE"
             , translate (-halfWidth + 10 + 80 * 10) (-halfHeight + height / 2 - 20) $ scale 0.1 0.1 $ color black $ Text "(4) - Select GliderGun SW"
             , translate (-halfWidth + 10 + 80 * 10) (-halfHeight + height / 2 - 40) $ scale 0.1 0.1 $ color black $ Text "(5) - Select GliderGun NW"
             ]
    where halfHeight = fromIntegral screenHeight / 2.0
          halfWidth = fromIntegral screenWidth / 2.0
          button :: String -> Picture
          button text = pictures [ color (greyN 0.7) $ rectangleSolid 70 30
                                 , translate (-30) (-5) $ scale 0.1 0.1 $ color black $ Text text
                                 ]


-- отрисовка экземпляра игры
drawGame :: Game -> Picture
drawGame game =
    pictures [ color black $ drawCells game
              , if showGrid game then color black $ drawGrid game else Blank
              , translate (-halfWidth + 10) (halfHeight - 40) $ scale 0.25 0.25 $ color red  $ text gameStateText
              , drawPanel newPanel (screenSize game)
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
         'g' -> if keyState == Down then game {showGrid     = not $ showGrid game} else game
         'r' -> if keyState == Down then game {board        = HS.empty, paused = True} else game
         '1' -> if keyState == Down then game {configChoice = Just GliderChoice} else game
         '2' -> if keyState == Down then game {configChoice = Just GliderGunSEChoice} else game
         '3' -> if keyState == Down then game {configChoice = Just GliderGunNEChoice} else game
         '4' -> if keyState == Down then game {configChoice = Just GliderGunSWChoice} else game
         '5' -> if keyState == Down then game {configChoice = Just GliderGunNWChoice} else game
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
        LeftButton -> if not $ isPanelClick position
                      then case configChoice game of
                           Just choice -> game { board = addConfiguration choice mouseToCellCoordinates (board game)
                               , configChoice = Nothing }  -- Добавляем выбранную конфигурацию на игровое поле и сбрасываем выбор
                           Nothing -> if not $ mouseToCellCoordinates `HS.member` board game
                               then game { board = HS.insert mouseToCellCoordinates (board game) }
                               else game { board = HS.delete mouseToCellCoordinates (board game) }
                      else processButtonClick game position
        _          -> game
    where mouseToCellCoordinates :: Cell
          mouseToCellCoordinates =
              ( floor $ (fst position / zoom gameCamera + x gameCamera) / cellSize + 1.0
              , floor $ (snd position / zoom gameCamera + y gameCamera) / cellSize + 1.0)

          gameCamera :: Camera
          gameCamera = camera game
          isPanelClick :: (Float, Float) -> Bool
          isPanelClick (xPos, yPos) = yPos < -halfHeight + panelHeight newPanel
          (width, height) = fromIntegral <$> screenSize game
          halfHeight = fromIntegral height / 2.0
          halfWidth = fromIntegral width / 2.0
          -- Обработка нажатия на кнопки на панели
          processButtonClick :: Game -> (Float, Float) -> Game
          processButtonClick game (clickX, clickY)
              | clickY >= (-halfHeight + 10 + 80 * 1 + 10) && clickY <= (-halfHeight + 10 + 80 * 1 + 40) =
                  case clickX of
                      _ | clickX >= (-halfWidth + 10 + 80 * 1 - 35) && clickX <= (-halfWidth + 10 + 80 * 1 + 35) -> handleButtonAction game "Start"
                        | clickX >= (-halfWidth + 10 + 80 * 2 - 35) && clickX <= (-halfWidth + 10 + 80 * 2 + 35) -> handleButtonAction game "g"
                        | clickX >= (-halfWidth + 10 + 80 * 3 - 35) && clickX <= (-halfWidth + 10 + 80 * 3 + 35) -> handleButtonAction game "r"
                        | clickX >= (-halfWidth + 10 + 80 * 4 - 35) && clickX <= (-halfWidth + 10 + 80 * 4 + 35) -> handleButtonAction game "NOT Gate"
                        | clickX >= (-halfWidth + 10 + 80 * 5 - 35) && clickX <= (-halfWidth + 10 + 80 * 5 + 35) -> handleButtonAction game "AND Gate"
                        | clickX >= (-halfWidth + 10 + 80 * 6 - 35) && clickX <= (-halfWidth + 10 + 80 * 6 + 35) -> handleButtonAction game "OR Gate"
                        | otherwise -> game  -- Нажатие было вне панели, игнорируем его
              | otherwise = game  -- Нажатие было вне панели, игнорируем его
              where handleButtonAction :: Game -> String -> Game
                    handleButtonAction game buttonSymbol =
                        case buttonSymbol of
                            "Start" -> game { paused = not $ paused game }
                            "g" -> game { showGrid = not $ showGrid game }
                            "r" -> game { board = HS.empty, paused = True }
                            "NOT Gate" -> game {configChoice = Just NOTGateChoice}
                            "AND Gate" -> game {configChoice = Just ANDGateChoice}
                            "OR Gate" -> game {configChoice = Just ORGateChoice}
                            _   -> game
                    (width, height) = fromIntegral <$> screenSize game
                    halfHeight =  height / 2.0
                    halfWidth = fromIntegral width / 2.0

gameInteract (EventResize newScreenSize) game = game {screenSize = newScreenSize}
gameInteract _ game = game

-- игровое состояние
data Game = Game { board      :: Board
                 , camera     :: Camera
                 , paused     :: Bool
                 , showGrid   :: Bool
                 , screenSize :: (Int, Int)
                 , configChoice    :: Maybe ConfigurationChoice
                 , configPosition  :: Maybe Cell
                 }

-- новая игра
newGame :: Game
newGame = Game { board  = HS.empty
               , camera = Camera { x = 0.0,    deltaX = 0.0
                                 , y = 0.0,    deltaY = 0.0
                                 , zoom = 3.0, deltaZoom = 0.0}
               , paused = True
               , showGrid = True
               , screenSize = defaultScreenSize
               , configChoice  = Nothing
               , configPosition = Nothing
               }


title :: String
title = "Conway's game of life"

defaultScreenSize :: (Int, Int)
defaultScreenSize = (1500, 800)

backgroundColor :: Color
backgroundColor = white

iterationsPerSecond :: Int
iterationsPerSecond = 50

moveSpeed :: Float
moveSpeed = 200.0

zoomSpeed :: Float
zoomSpeed = 1.0

cellSize :: Float
cellSize = 10.0

main :: IO ()
main = play (InWindow title defaultScreenSize (1500, 800)) backgroundColor iterationsPerSecond newGame drawGame gameInteract iterateGame

-- stack build
-- stack run
