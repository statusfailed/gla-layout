-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

import Types
import Drawing

-- | Miso framework import
import Miso
import Miso.String

-- | Diagram in the form t0;t1;t2
-- where each t_i is in the form (a (+)  (+) ... (+) z)
newtype Diagram = Diagram [[Generator]]

-- | Type synonym for an application model
type Model = ()

-- | Sum type for application events
data Action = Action
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Action -- initial action to be executed on application load
    model  = ()            -- initial model
    update = updateModel   -- update function
    view   = viewModel     -- view function
    events = defaultEvents -- default delegated events
    subs   = []            -- empty subscription list
    mountPoint = Nothing   -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Action () = pure ()

viewModel :: Model -> View Action
viewModel x = div_ [] [ viewSVG ]
