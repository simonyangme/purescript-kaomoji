module Component.List where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Array ((:))
import Data.Foreign.Class (IsForeign, readProp)
import Data.Functor (($>))
import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.String (null)

import Halogen (ChildF(), Component(), EvalParent(), HalogenEffects(), InstalledState(), gets, modify, liftAff', liftH, parentComponent, ParentHTML())
import Halogen.Query (action)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), get)
import Control.Monad.Eff.Console (CONSOLE())

import Component.Entry
import Model

data ListQuery a = AddEntry a
                 | UpdateKeyword String a

newtype EntrySlot = EntrySlot EntryId
derive instance genericEntrySlot :: Generic EntrySlot
instance eqEntrySlot :: Eq EntrySlot where eq = gEq
instance ordEntrySlot :: Ord EntrySlot where compare = gCompare

type StateP g = InstalledState List Entry ListQuery EntryQuery g EntrySlot
type QueryP = Coproduct ListQuery (ChildF EntrySlot EntryQuery)

type AppEffects eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)

ui :: forall eff. Component (StateP (Aff (AppEffects eff))) QueryP (Aff (AppEffects eff))
ui = parentComponent render eval
  where

  render :: forall g. (Functor g) => List -> ParentHTML Entry ListQuery EntryQuery g EntrySlot
  render st =
    H.div_
      [ H.h1_
          [ H.text "(༶ૢ˃̵̑◡˂̵̑༶ૢ)"]
      , H.form [ E.onSubmit (\_ -> EH.preventDefault $> action AddEntry) ]
         [ H.input [ P.inputType P.InputText
                   , P.placeholder "Keyword"
                   , P.autofocus true
                   , P.value st.keyword
                   , E.onValueChange (E.input UpdateKeyword)
                   ]
         , H.input [ P.inputType P.InputSubmit
                   , P.value "顔文字"
                   ]
         ]
      , H.ul_ (map renderEntry st.entries)
      ]

  renderEntry :: forall g. (Functor g) => Entry -> ParentHTML Entry ListQuery EntryQuery g EntrySlot
  renderEntry e = H.slot (EntrySlot e.id) \_ -> { component: entry, initialState: e }

  eval :: forall g. EvalParent ListQuery List Entry ListQuery EntryQuery (Aff (ajax :: AJAX, console :: CONSOLE | g)) EntrySlot
  eval (AddEntry next) = do
    nextId <- gets _.nextId
    modify incId
    keyword <- gets _.keyword
    result <- liftH $ liftAff' $ (fetchKeyword nextId $ Just keyword)
    modify <<< addEntry $ { keyword: if null keyword then "random" else keyword, kaomoji: result, id: nextId }
    pure next
  eval (UpdateKeyword kw next) = do
    modify (_ { keyword = kw })
    pure next

addEntry :: Entry -> List -> List
addEntry e l = l { entries = e : l.entries }

incId :: List -> List
incId l = l { nextId = l.nextId + 1 }

fetchKeyword :: forall eff. EntryId -> Maybe String -> Aff (ajax :: AJAX | eff) String
fetchKeyword eid keyword = do
  result <- get $ "./entry?keyword=" ++ maybe "" id keyword ++ "&" ++ show eid
  let response = result.response
  return case readProp "kaomojiText" response of
    Right t -> t
    Left _ -> "Invalid response"
