module Model where

import Prelude

import Data.Foreign.Class (IsForeign, read, readProp)

type EntryId = Int

type Entry = { keyword :: String, kaomoji :: String, id :: EntryId }

type List = { entries :: Array Entry, keyword :: String, nextId :: EntryId }

initialEntry :: Entry
initialEntry = { keyword: "INITIAL", kaomoji: "ENTRY", id: 0 }

initialList :: List
initialList = { entries: [], keyword: "cat", nextId: 0 }


data KaomojiResponse = KaomojiResponse
                       { originalCategories :: Array String
                       , keywords :: Array String
                       , kaomojiText :: String }
instance kaomojiIsForeign :: IsForeign KaomojiResponse where
  read value = do
    o <- readProp "originalCategories" value
    kw <- readProp "keywords" value
    kt <- readProp "kaomojiText" value
    return $ KaomojiResponse { originalCategories: o
                             , keywords: kw
                             , kaomojiText: kt }
