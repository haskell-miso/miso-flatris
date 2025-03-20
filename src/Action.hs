module Action where

import Miso
import Miso.String (MisoString)

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
