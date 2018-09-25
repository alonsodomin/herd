{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Data.Text where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText Char where
  toText = T.singleton

instance ToText String where
  toText = T.pack

instance ToText Int where
  toText = T.pack . show

instance ToText UTCTime where
  toText utc = T.pack $ formatTime defaultTimeLocale "%m/%d/%Y" utc
