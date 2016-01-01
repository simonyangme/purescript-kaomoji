module Component.Entry where

import Prelude

-- import Data.Const (Const(), getConst)
-- import Data.Void (Void(), absurd)

import Halogen (Component(), component, ComponentDSL(), ComponentHTML(), Natural())
import qualified Halogen.HTML.Indexed as H
-- import qualified Halogen.HTML.Properties.Indexed as P
-- import qualified Halogen.HTML.Events.Indexed as E

import Model

-- TODO: Try to figure out how to use this with Const Void
data EntryQuery a = EntryQuery a

entry :: forall g. (Functor g) => Component Entry EntryQuery g
entry = component render eval
  where

  render :: Entry -> ComponentHTML EntryQuery
  render e =
    H.div_ [H.text $ e.keyword ++ " : " ++ e.kaomoji]

  eval :: Natural EntryQuery (ComponentDSL Entry EntryQuery g)
  eval (EntryQuery next) = pure next
