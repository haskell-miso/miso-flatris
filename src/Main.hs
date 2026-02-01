-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell module declaration
module Main where

import System.Random

-- | Miso framework import
import Miso

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
  startApp (defaultEvents <> mouseEvents) (component model updateModel viewModel)
    { mount = Just Init
    , subs = [arrowsSub GetArrows]
    , logLevel = DebugAll
    } 
