module Component.Entry where

import Prelude

import Data.Const (Const(), getConst)
import Data.Void (Void(), absurd)

import Halogen (Component(), component, ComponentDSL(), ComponentHTML(), Natural())
import qualified Halogen.HTML.Indexed as H

import Model

type EntryQuery = Const Void

entry :: forall g. (Functor g) => Component Entry EntryQuery g
entry = component render eval
  where

  render :: Entry -> ComponentHTML EntryQuery
  render e =
    H.div_ [H.text $ e.keyword ++ " : " ++ e.kaomoji]

  eval :: Natural EntryQuery (ComponentDSL Entry EntryQuery g)
  eval = absurd <<< getConst
