-- | Haskell language pragma
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell module declaration
module Main where

import Control.Arrow
import qualified Data.Map.Lazy as M
import System.Random

-- | Miso framework import
import Miso
import qualified Miso.String as S

import Action
import Model
import Update
import View

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = do
  t <- now
  gen <- getStdGen
  let (tetro, nGen) = random gen
      seed = fst . random $ nGen :: Int
      model = initialModel {time = t, nextTetro = tetro, randSeed = seed}
  startComponent (defaultComponent model updateModel viewModel)
    { initialAction = Just Init
    , subs = [arrowsSub GetArrows]
    , logLevel = DebugAll
    , events = defaultEvents <> mouseEvents
    } 
