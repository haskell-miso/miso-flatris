{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Update where

import Control.Monad.State hiding (state)
import Data.Function
import Grid
import Miso

import Action
import Model
import Tetromino

-- | Updates model, optionally introduces side effects
updateModel
  :: Action
  -> Transition Model Action
updateModel Resume = do
  modify $ \model -> model
    { state = Playing
    , fall = (fall model) { isActive = True }
    }
updateModel Start = do
  (newNext, newSeed) <- randomTetro . randSeed <$> get
  put $ initialModel
    { state = Playing
    , fall = defaultFall { isActive = True }
    , randSeed = newSeed
    , nextTetro = newNext
    } & spawnTetromino
updateModel Pause = do
  modify $ \model -> model
    { state = Paused
    , fall = (fall model) { isActive = False }
    }
updateModel Rotate = do
  modify $ \model -> model
    { rotation = (rotation model)
      { isActive = True
      }
    }
updateModel MoveLeft = do
  modify $ \model -> model
    { movement = (movement model) { isActive = True }
    , arrows = (\(_, y) -> (-1, y)) (arrows model)
    }
updateModel MoveRight = do
  modify $ \model -> model
    { movement = (movement model) { isActive = True }
    , arrows = (\(_, y) -> (1, y)) (arrows model)
    }
updateModel (Time newTime) = do
  m <- get
  step m
    { delta = newTime - time m
    , time = newTime
    }
updateModel (GetArrows Arrows {..}) = do
  modify $ \m ->
    m { arrows = (arrowX, arrowY)
      } & checkArrows
updateModel Init =
  io (Time <$> now)
updateModel _ = pure ()

checkArrows :: Model -> Model
checkArrows model@Model {..} =
  case state of
    Playing -> model & checkMovement & checkRotation & checkDrop
    _ -> model

checkMovement :: Model -> Model
checkMovement model@Model {..} = model {movement = newMovement}
  where
    newMovement = movement {isActive = isActive movement || fst arrows /= 0}

checkRotation :: Model -> Model
checkRotation model@Model {..} = model {rotation = newRotation}
  where
    newRotation = rotation {isActive = isActive rotation || snd arrows == 1}

checkDrop :: Model -> Model
checkDrop model@Model {..} = model {fall = newFall}
  where
    newDelay =
      if snd arrows == -1
        then 30
        else 800
    newFall = fall {delay = newDelay}

step :: Model -> Transition Model Action
step model = k <# (Time <$> now)
  where
    k = shouldStep model

shouldStep :: Model -> Model
shouldStep model@Model {..} =
  case state of
    Playing -> k
    _ -> model
  where
    k =
      model & updateAnimation & moveTetromino & rotateTetromino & dropTetromino &
      checkEndGame

updateAnimation :: Model -> Model
updateAnimation model@Model {..} =
  model {rotation = newRotation, movement = newMovement, fall = newFall}
  where
    newRotation = updateAnimation_ time rotation
    newMovement = updateAnimation_ time movement
    newFall = updateAnimation_ time fall

updateAnimation_ :: Double -> AnimationState -> AnimationState
updateAnimation_ time state@AnimationState {..} =
  state {ticks = newTicks, isAnimated = newAnimated}
  where
    newTicks = getTicks time delay
    newAnimated = newTicks /= ticks

getTicks :: Double -> Int -> Int
getTicks time delay = floor time `div` delay

moveTetromino :: Model -> Model
moveTetromino model@Model {..} =
  if isAnimated movement && isActive movement
    then move_ model
    else model

move_ :: Model -> Model
move_ model@Model {..} = model {movement = newMovement, x = newX}
  where
    x_ = x + fst arrows
    newMovement = movement {isActive = False}
    newX =
      if collide width height x_ y (fromList "" . shapeToCoord $ active) grid
        then x
        else x_

rotateTetromino :: Model -> Model
rotateTetromino model@Model {..} =
  if isAnimated rotation && isActive rotation
    then shiftPosition [0, 1, -1, 2, -2] model
    else model

shiftPosition :: [Int] -> Model -> Model
shiftPosition [] model = model
shiftPosition (dx:dxs) model@Model {..} =
  if collide width height x_ y (activeGrid newActive color) grid
    then shiftPosition dxs model
    else model {x = x_, active = newActive, rotation = newRotation}
  where
    x_ = x + dx
    newActive = rotate active
    newRotation = rotation {isActive = False}

dropTetromino :: Model -> Model
dropTetromino model@Model {..} =
  if isAnimated fall && isActive fall
    then drop_ model
    else model

drop_ :: Model -> Model
drop_ model@Model {..} =
  if collide width height x y_ (activeGrid active color) grid
  then
    model
    { grid = stamp x y (activeGrid active color) grid
    , score = score + s
    } & spawnTetromino & clearLines_
  else
    model { y = y_ }
  where
    y_ = y + 1
    s = [4, 8] !! (abs . snd $ arrows)

spawnTetromino :: Model -> Model
spawnTetromino model@Model {..} =
  model
  { x = (width `div` 2) - 2
  , y = -2
  , nextTetro = newNext
  , active = newActive
  , color = newColor
  , randSeed = newStdGen
  }
  where
    (newNext, newStdGen) = randomTetro randSeed
    newActive = tetroShape nextTetro
    newColor = tetroColor nextTetro

clearLines_ :: Model -> Model
clearLines_ model@Model {..} =
  model
  { grid = newGrid
  , linesCleared = linesCleared + lc
  , score = score + bonus !! lc
  }
  where
    (newGrid, lc) = clearLines width grid
    bonus = [0, 100, 300, 500, 800]

checkEndGame :: Model -> Model
checkEndGame model@Model {..} =
  if or . mapToList (\_ (y, _) -> y < 0) $ grid
    then model {state = Stopped}
    else model
