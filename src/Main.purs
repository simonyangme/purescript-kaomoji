module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen (HalogenEffects(), installedState, runUI)
import Halogen.Util (appendToBody, onLoad)

import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Console (CONSOLE())

import Component.List as L
import Model

type State = { results :: Array String }

initialState :: State
initialState = { results: [] }

data Query a
  = MakeRequest a

type AppEffects eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI L.ui (installedState initialList)
  onLoad $ appendToBody app.node
