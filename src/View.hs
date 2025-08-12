{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import Control.Arrow
import Data.Aeson.Encode.Pretty

import           Miso
import qualified Miso.Style as CSS

import Action
import Grid
import Model
import Tetromino

shapeList :: Char -> [(MisoString, MisoString)]
shapeList 'Z' = [("0%", "0%"), ("0%", "25%"), ("25%", "25%"), ("25%", "50%")]
shapeList _ = []

renderSquare ::
     (MisoString, MisoString)
  -> ((MisoString, MisoString), MisoString)
  -> View model Action
renderSquare (width, height) ((top, left_), color) =
  li_
    [ class_ "grid-square-block"
    , CSS.style_
      [ ("top", top)
      , ("left", left_)
      , ("width", width)
      , ("height", height)
      , ("position", "absolute")
      ]
    ]
    [ div_
        [ class_ "square-block"
        , CSS.style_
          [ ("background-color", color), ("width", "100%"), ("height", "100%") ]
        ]
        []
    ]

renderTetromino :: [[Int]] -> MisoString -> View model Action
renderTetromino shape color =
  ul_
    [ class_ "tetromino"
    , CSS.style_
      [ ("margin", "0")
      , ("padding", "0")
      , ("list-style-type", "none")
      , ("position", "relative")
      , ("width", "100%")
      , ("height", "100%")
      ]
    ]
    (map (renderSquare ("25%", "25%") . (, color) . conv') $ shapeToCoord shape)
  where
    conv = ms . (++ "%") . show . (* 25)
    conv' = conv *** conv

renderNext :: Model -> View model Action
renderNext Model {..} =
  div_
    [ class_ "next-tetromino"
    , CSS.style_
      [ ("padding", "0px")
      , ("margin-top", "8px")
      , ("overflow", "hidden")
      , ("position", "relative")
      , ("width", "92px")
      , ("height", "92px")
      ]
    ]
    [flip renderTetromino "#ecf0f1" . tetroShape $ nextTetro]

renderActive :: Model -> View model Action
renderActive Model {..} =
  div_
    [ class_ "active-tetromino"
    , CSS.style_
      [ ("top", conv (5 * y))
      , ("left", conv (10 * x))
      , ("width", "40%")
      , ("height", "20%")
      , ("position", "absolute")
      ]
    ]
    [renderTetromino active color]
  where
    conv = ms . (++ "%") . show

renderGrid :: Model -> View model Action
renderGrid Model {..} =
  ul_
    [ class_ "well-grid"
    , CSS.style_
      [ ("position", "relative")
      , ("width", "100%")
      , ("height", "100%")
      , ("margin", "0")
      , ("padding", "0")
      , ("list-style-type", "none")
      ]
    ]
    (map (renderSquare ("10%", "5%") . conv_) grid)
  where
    conv = ms . (++ "%") . show
    conv' = (conv . (* 5)) *** (conv . (* 10))
    conv_ Cell {..} = (conv' pos, value)

renderWell :: Model -> View model Action
renderWell model =
  div_
    [ class_ "well-container"
    , CSS.style_
      [ ("position", "absolute")
      , ("top", "0")
      , ("left", "0")
      , ("background", "#ecf0f1")
      , ("padding", "0px")
      , ("margin", "0px")
      , ("overflow", "hidden")
      , ("width", "300px")
      , ("height", "600px")
      ]
    ]
    [ div_
        [ class_ "well"
        , CSS.style_
          [ ("position", "relative")
          , ("width", "100%")
          , ("height", "100%")
          , ("overflow", "hidden")
          , ("display", "block")
          ]
        ]
        [renderGrid model, renderActive model]
    ]

renderControlButton :: MisoString -> Action -> View model Action
renderControlButton txt act =
  div_
    [ CSS.style_
      [ ("background", "#ecf0f1")
      , ("border", "0")
      , ("color", "#34495f")
      , ("cursor", "pointer")
      , ("text-align", "center")
      , ("-webkit-user-select", "none")
      , ("display", "block")
      , ("float", "left")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "24px")
      , ("font-weight", "300")
      , ("height", "60px")
      , ("line-height", "60px")
      , ("margin", "20px 20px 0 0")
      , ("outline", "none")
      , ("padding", "0")
      , ("width", "60px")
      ]
    , onMouseDown act
    ]
    [text txt]

renderControls :: View model Action
renderControls =
  div_
    [ class_ "controls"
    , CSS.style_
      [ ("height", "8%")
      , ("left", "0")
      , ("position", "absolute")
      , ("top", "600px")
      ]
    ]
    [ renderControlButton "↻" Rotate
    , renderControlButton "←" MoveLeft
    , renderControlButton "→" MoveRight
    , renderControlButton "↓" Accelerate
    ]

renderTitle :: MisoString -> View model Action
renderTitle title =
  div_
    [ CSS.style_
      [ ("color", "#34495f")
      , ("font-size", "40px")
      , ("line-height", "60px")
      , ("margin", "30px 0 0")
      ]
    ]
    [text title]

renderLabel :: MisoString -> View model Action
renderLabel label =
  div_
    [ CSS.style_
      [ ("color", "#bdc3c7")
      , ("font-weight", "300")
      , ("line-height", "1")
      , ("margin", "30px 0 0")
      ]
    ]
    [text label]

renderCount :: Int -> View model Action
renderCount count =
  div_
    [ CSS.style_
      [ ("color", "#3993d0")
      , ("font-size", "30px")
      , ("line-height", "1")
      , ("margin", "5px 0 0")
      ]
    ]
    [text . ms . show $ count]

renderGameButton :: State -> View model Action
renderGameButton state =
  let (txt, action) =
        case state of
          Paused -> ("Resume", Resume)
          Playing -> ("Pause", Pause)
          Stopped -> ("New Game", Start)
  in button_
       [ onClick action
       , CSS.style_
         [ ("background", "#34495f")
         , ("border", "0")
         , ("bottom", "30px")
         , ("color", "#fff")
         , ("cursor", "pointer")
         , ("display", "block")
         , ("font-family", "Helvetica, Arial, sans-serif")
         , ("font-size", "18px")
         , ("font-weight", "300")
         , ("height", "60px")
         , ("left", "30px")
         , ("line-height", "60px")
         , ("outline", "none")
         , ("padding", "0")
         , ("position", "auto")
         , ("width", "120px")
         ]
       ]
       [text txt]

renderPanel :: Model -> View model Action
renderPanel model@Model {..} =
  div_
    [ class_ "game-panel-container"
    , CSS.style_
      [ ("bottom", "80px")
      , ("color", "#34495f")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "14px")
      , ("left", "300px")
      , ("padding", "0 30px")
      , ("position", "absolute")
      , ("right", "0")
      , ("top", "0")
      ]
    ]
    [ renderTitle "Flatris"
    , renderLabel "Score"
    , renderCount score
    , renderLabel "Lines Cleared"
    , renderCount linesCleared
    , renderLabel "Next Shape"
    , div_
        [ class_ "next"
        , CSS.style_ [ ("margin-top", "10px"), ("position", "relative") ]
        ]
        [renderNext model]
    , renderGameButton state
    ]

renderInfo :: State -> View model Action
renderInfo state =
  div_
    [ class_ "info-panel-container"
    , CSS.style_
      [ ("background", "rgba(236, 240, 241, 0.85)")
      , ("color", "#34495f")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "18px")
      , ("height", "600px")
      , ("left", "0")
      , ("line-height", "1.5")
      , ("padding", "0 15px")
      , ("position", "absolute")
      , ("top", "0")
      , ("width", "270px")
      , ("display", display)
      ]
    ]
    [ p_
        []
        [ text "hs-flatris is a "
        , b_ [] [text "Flatris "]
        , text "clone coded by "
        , a_ [href_ "https://github.com/ptigwe", target_ "_blank"] ["@ptigwe"]
        , " in Haskell using the "
        , a_
            [href_ "https://github.com/haskell-miso/miso", target_ "_blank"]
            ["Miso"]
        , " library"
        ]
    , p_
        []
        [ text "Inspired by the classic "
        , b_ [] [text "Tetris "]
        , text
            "game, the game can be played with a keyboard using the arrow keys, and on mobile devices using the buttons below."
        ]
    ]
  where
    display =
      case state of
        Playing -> "none"
        _ -> "block"

renderView :: Model -> View model Action
renderView model@Model {..} =
  div_
    [class_ "flatris-game"]
    [renderWell model, renderControls, renderPanel model, renderInfo state]

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View model Action
viewModel model =
  div_
    [ onMouseUp UnlockButtons
    , id_ "root"
    , CSS.style_ [("padding", "30px 0")]
    ]
    [ div_
        [ class_ "game"
        , CSS.style_
          [ ("height", "680px")
          , ("margin", "auto")
          , ("position", "relative")
          , ("width", "480px")
          ]
        ]
        [renderView model]
    , div_
        [ class_ "preview"
        , CSS.style_
          [ ("left", "5px")
          , ("top", "30px")
          , ("overflow", "scroll")
          , ("width", "30%")
          , ("height", "680px")
          , ("position", "absolute")
          , ("background-color", "#34495f")
          , ("color", "#fff")
          ]
        ]
        [pre_ [] [text . ms . encodePretty $ model]]
    ]
