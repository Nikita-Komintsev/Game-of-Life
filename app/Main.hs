{-# LANGUAGE ParallelListComp #-}

module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List as L
import Data.Fixed
import Data.HashSet as HS
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Point

--ячейка на доске
type Cell = (Int, Int)
--Любые ячейки, присутствующие в HashSet, живы, а все остальные мертвы.
type Board = HashSet Cell

data Configuration = Glider | GliderGun | GliderGun2 | GliderGun90 | GliderGun902 | NOTGate | ANDGate | ORGate

initialBoard :: Configuration -> Board
initialBoard Glider = HS.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (1, 2)]
initialBoard GliderGun = HS.fromList [(12,-3), (13,-3), (11,-2), (15,-2), (10,-1), (16,-1), (24,-1), (0,0), (1,0), (10,0), (14,0), (16,0), (17,0), (22,0), (24,0), (0,1), (1,1), (10,1), (16,1), (20,1), (21,1), (11,2), (15,2), (20,2), (21,2), (34,2), (35,2), (12,3), (13,3), (20,3), (21,3), (34,3), (35,3), (22,4), (24,4), (24,5)]
initialBoard GliderGun2 = HS.fromList [(-12,-3), (-13,-3), (-11,-2), (-15,-2), (-10,-1), (-16,-1), (-24,-1), (0,0), (-1,0), (-10,0), (-14,0), (-16,0), (-17,0), (-22,0), (-24,0), (0,1), (-1,1), (-10,1), (-16,1), (-20,1), (-21,1), (-11,2), (-15,2), (-20,2), (-21,2), (-34,2), (-35,2), (-12,3), (-13,3), (-20,3), (-21,3), (-34,3), (-35,3), (-22,4), (-24,4), (-24,5)]
initialBoard GliderGun90 = HS.fromList [(0, 0), (1, 0), (0, 1), (1, 1), (0, 10), (1, 10), (2, 10), (-1, 11), (3, 11), (-2, 12), (4, 12), (-2, 13), (4, 13), (1, 14), (-1, 15), (3, 15), (0, 16), (1, 16), (2, 16), (1, 17), (-2, 20), (-1, 20), (0, 20), (-2, 21), (-1, 21), (0, 21), (-3, 22), (1, 22), (-4, 24), (-3, 24), (1, 24), (2, 24), (-2, 34), (-1, 34), (-2, 35), (-1, 35)]
initialBoard GliderGun902 = HS.fromList [(0, 0), (-1, 0), (0, 1), (-1, 1), (0, 10), (-1, 10), (-2, 10), (1, 11), (-3, 11), (2, 12), (-4, 12), (2, 13), (-4, 13), (-1, 14), (1, 15), (-3, 15), (0, 16), (-1, 16), (-2, 16), (-1, 17), (2, 20), (1, 20), (0, 20), (2, 21), (1, 21), (0, 21), (3, 22), (-1, 22), (4, 24), (3, 24), (-1, 24), (-2, 24), (2, 34), (1, 34), (2, 35), (1, 35)]

initialBoard NOTGate = HS.fromList [(12,-3), (13,-3), (11,-2), (15,-2), (10,-1), (16,-1), (24,-1), (0,0), (1,0), (10,0), (14,0), (16,0), (17,0), (22,0), (24,0), (0,1), (1,1), (10,1), (16,1), (20,1), (21,1), (11,2), (15,2), (20,2), (21,2), (34,2), (35,2), (12,3), (13,3), (20,3), (21,3), (34,3), (35,3), (22,4), (24,4), (24,5),
                                    (15, -48),  (16, -48),  (15, -47),  (16, -47),  (15, -38),  (16, -38),  (17, -38),  (14, -37),  (18, -37),  (13, -36),  (19, -36),  (13, -35),  (19, -35),  (16, -34),  (14, -33),  (18, -33),  (15, -32),  (16, -32),  (17, -32),  (16, -31),  (13, -28),  (14, -28),  (15, -28),  (13, -27),  (14, -27),  (15, -27),  (12, -26),  (16, -26),  (11, -24),  (12, -24),  (16, -24),  (17, -24),  (13, -14),  (14, -14),  (13, -13),  (14, -13),
                                    (23, 43), (24, 43), (23, 44), (24, 44), (23, 53), (24, 53), (25, 53), (22, 54), (26, 54), (21, 55), (27, 55), (21, 56), (27, 56), (24, 57), (22, 58), (26, 58), (23, 59), (24, 59), (25, 59), (24, 60), (21, 63), (22, 63), (23, 63), (21, 64), (22, 64), (23, 64), (20, 65), (24, 65), (19, 67), (20, 67), (24, 67), (25, 67), (21, 77), (22, 77), (21, 78), (22, 78),
                                    (72, 269), (73, 269), (71, 270), (75, 270), (70, 271), (76, 271), (84, 271), (60, 272), (61, 272), (70, 272), (74, 272), (76, 272), (77, 272), (82, 272), (84, 272), (60, 273), (61, 273), (70, 273), (76, 273), (80, 273), (81, 273), (71, 274), (75, 274), (80, 274), (81, 274), (94, 274), (95, 274), (72, 275), (73, 275), (80, 275), (81, 275), (94, 275), (95, 275), (82, 276), (84, 276), (84, 277),
                                    (-83, 263), (-82, 263), (-83, 264), (-82, 264)]

initialBoard ANDGate = HS.fromList [(12,-3), (13,-3), (11,-2), (15,-2), (10,-1), (16,-1), (24,-1), (0,0), (1,0), (10,0), (14,0), (16,0), (17,0), (22,0), (24,0), (0,1), (1,1), (10,1), (16,1), (20,1), (21,1), (11,2), (15,2), (20,2), (21,2), (34,2), (35,2), (12,3), (13,3), (20,3), (21,3), (34,3), (35,3), (22,4), (24,4), (24,5),
                                   (282, -6), (281, -6), (283, -5), (279, -5), (284, -4), (278, -4), (270, -4), (294, -3), (293, -3), (284, -3), (280, -3), (278, -3), (277, -3), (272, -3), (270, -3), (294, -2), (293, -2), (284, -2), (278, -2), (274, -2), (273, -2), (283, -1), (279, -1), (274, -1), (273, -1), (260, -1), (259, -1), (282, 0), (281, 0), (274, 0), (273, 0), (260, 0), (259, 0), (272, 1), (270, 1), (270, 2),
                                   (87, -152), (86, -152), (87, -151), (86, -151), (87, -142), (86, -142), (85, -142), (88, -141), (84, -141), (89, -140), (83, -140), (89, -139), (83, -139), (86, -138), (88, -137), (84, -137), (87, -136), (86, -136), (85, -136), (86, -135), (89, -132), (88, -132), (87, -132), (89, -131), (88, -131), (87, -131), (90, -130), (86, -130), (91, -128), (90, -128), (86, -128), (85, -128), (89, -118), (88, -118), (89, -117), (88, -117),
                                   (-111, -34), (-110, -34), (-111, -33), (-110, -33), (-111, -24), (-110, -24), (-109, -24), (-112, -23), (-108, -23), (-113, -22), (-107, -22), (-113, -21), (-107, -21), (-110, -20), (-112, -19), (-108, -19), (-111, -18), (-110, -18), (-109, -18), (-110, -17), (-113, -14), (-112, -14), (-111, -14), (-113, -13), (-112, -13), (-111, -13), (-114, -12), (-110, -12), (-115, -10), (-114, -10), (-110, -10), (-109, -10), (-113, 0), (-112, 0), (-113, 1), (-112, 1),
                                   (-138, 25), (-137, 25), (-138, 26), (-137, 26), (-138, 35), (-137, 35), (-136, 35), (-139, 36), (-135, 36), (-140, 37), (-134, 37), (-140, 38), (-134, 38), (-137, 39), (-139, 40), (-135, 40), (-138, 41), (-137, 41), (-136, 41), (-137, 42), (-140, 45), (-139, 45), (-138, 45), (-140, 46), (-139, 46), (-138, 46), (-141, 47), (-137, 47), (-142, 49), (-141, 49), (-137, 49), (-136, 49), (-140, 59), (-139, 59), (-140, 60), (-139, 60),
                                   (-17, 210), (-16, 210), (-18, 211), (-14, 211), (-19, 212), (-13, 212), (-5, 212), (-29, 213), (-28, 213), (-19, 213), (-15, 213), (-13, 213), (-12, 213), (-7, 213), (-5, 213), (-29, 214), (-28, 214), (-19, 214), (-13, 214), (-9, 214), (-8, 214), (-18, 215), (-14, 215), (-9, 215), (-8, 215), (5, 215), (6, 215), (-17, 216), (-16, 216), (-9, 216), (-8, 216), (5, 216), (6, 216), (-7, 217), (-5, 217), (-5, 218),
                                   (178, 137), (177, 137), (179, 138), (175, 138), (180, 139), (174, 139), (166, 139), (190, 140), (189, 140), (180, 140), (176, 140), (174, 140), (173, 140), (168, 140), (166, 140), (190, 141), (189, 141), (180, 141), (174, 141), (170, 141), (169, 141), (179, 142), (175, 142), (170, 142), (169, 142), (156, 142), (155, 142), (178, 143), (177, 143), (170, 143), (169, 143), (156, 143), (155, 143), (168, 144), (166, 144), (166, 145),
                                   (53,22),(54,22),(54,23),(54,24),(55,25),(56,25),(56,24),
                                   (-214, 235), (-213, 235), (-214, 236), (-213, 236),
                                   (75, 169), (75, 170), (76, 169),(77, 170), (77,171), (77,172), (78,172),
                                   (-124, -198), (-123, -198), (-124, -197), (-123, -197)]



initialBoard ORGate = HS.fromList [(0,1),(1,1)]

-- Тип данных для представления выбора конфигурации
data ConfigurationChoice = GliderChoice | GliderGunChoice | GliderGunChoice2 | GliderGun90Choice| GliderGun90Choice2 | NOTGateChoice | ANDGateChoice | ORGateChoice deriving (Eq)

-- Функция для добавления выбранной конфигурации на игровое поле
addConfiguration :: ConfigurationChoice -> Cell -> Board -> Board
addConfiguration GliderChoice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard Glider
addConfiguration GliderGunChoice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard GliderGun
addConfiguration GliderGunChoice2 position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard GliderGun2
addConfiguration GliderGun90Choice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard GliderGun90
addConfiguration GliderGun90Choice2 position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard GliderGun902
addConfiguration NOTGateChoice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard NOTGate
addConfiguration ANDGateChoice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard ANDGate
addConfiguration ORGateChoice position board = HS.union board $ HS.map (\(x, y) -> (x + fst position, y + snd position)) $ initialBoard ORGate


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
                 , translate (-halfWidth + 10 + 80 * 13) (-halfHeight + height / 2 + 40) $ button "+ speed"
                 , translate (-halfWidth + 10 + 80 * 14) (-halfHeight + height / 2 + 40) $ button "- speed"
                 ]
    where halfHeight = fromIntegral screenHeight / 2.0
          halfWidth = fromIntegral screenWidth / 2.0
          button :: String -> Picture
          button text =
              pictures [ color (greyN 0.7) $ rectangleSolid 70 30
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
         'g' -> if keyState == Down then game {showGrid = not $ showGrid game} else game
         'r' -> if keyState == Down then game {board = HS.empty, paused = True} else game
         '1' -> if keyState == Down then game {configChoice = Just GliderChoice} else game  -- Выбор конфигурации Glider
         '2' -> if keyState == Down then game {configChoice = Just GliderGunChoice} else game  -- Выбор конфигурации GliderGun
         '3' -> if keyState == Down then game {configChoice = Just GliderGun90Choice} else game
         '4' -> if keyState == Down then game {configChoice = Just GliderGunChoice2} else game
         '5' -> if keyState == Down then game {configChoice = Just GliderGun90Choice2} else game
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
                        _ | clickX >= (-halfWidth + 10 + 80 * 1 - 35) && clickX <= (-halfWidth + 10 + 80 * 1 + 35) -> handleButtonAction game "Start"  -- Нажата кнопка старт / стоп
                          | clickX >= (-halfWidth + 10 + 80 * 2 - 35) && clickX <= (-halfWidth + 10 + 80 * 2 + 35) -> handleButtonAction game "g"  -- Нажата кнопка показа сетки
                          | clickX >= (-halfWidth + 10 + 80 * 3 - 35) && clickX <= (-halfWidth + 10 + 80 * 3 + 35) -> handleButtonAction game "r"  -- Нажата кнопка очистки поля
                          | clickX >= (-halfWidth + 10 + 80 * 4 - 35) && clickX <= (-halfWidth + 10 + 80 * 4 + 35) -> handleButtonAction game "NOT Gate"
                          | clickX >= (-halfWidth + 10 + 80 * 5 - 35) && clickX <= (-halfWidth + 10 + 80 * 5 + 35) -> handleButtonAction game "AND Gate"
                          | clickX >= (-halfWidth + 10 + 80 * 6 - 35) && clickX <= (-halfWidth + 10 + 80 * 6 + 35) -> handleButtonAction game "OR Gate"
                          | clickX >= (-halfWidth + 10 + 80 * 13 - 35) && clickX <= (-halfWidth + 10 + 80 * 13 + 35) -> handleButtonAction game "+ speed"
                          | clickX >= (-halfWidth + 10 + 80 * 14 - 35) && clickX <= (-halfWidth + 10 + 80 * 14 + 35) -> handleButtonAction game "- speed"
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
                          "+ speed" -> game
                          "- speed" -> game
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
                 , configChoice    :: Maybe ConfigurationChoice  -- Добавляем выбор конфигурации
                 , configPosition  :: Maybe Cell                 -- Позиция выбранной конфигурации
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
moveSpeed = 120.0

zoomSpeed :: Float
zoomSpeed = 1.0

cellSize :: Float
cellSize = 10.0

main :: IO ()
main = play (InWindow title defaultScreenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame
