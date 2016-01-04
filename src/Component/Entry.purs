module Component.Entry where

import Prelude

import Data.Const (Const(), getConst)
import Data.Void (Void(), absurd)

import Halogen (Component(), component, ComponentDSL(), ComponentHTML(), Natural())
import qualified Halogen.HTML.Indexed as H

import Model

type Query = Const Void

ui :: forall g. (Functor g) => Component Entry Query g
ui = component render eval
  where

  render :: Entry -> ComponentHTML Query
  render e =
    H.div_ [H.text $ e.keyword ++ " : " ++ e.kaomoji]

  eval :: Natural Query (ComponentDSL Entry Query g)
  eval = absurd <<< getConst
