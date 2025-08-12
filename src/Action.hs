module Action where

import Miso

data Action
  = Init
  | Load MisoString
  | Start
  | Pause
  | Resume
  | UnlockButtons
  | MoveLeft
  | MoveRight
  | Rotate
  | Accelerate
  | GetArrows Arrows
  | Time Double
  | Noop
  deriving (Show, Eq)
