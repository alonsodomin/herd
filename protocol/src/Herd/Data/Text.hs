{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Data.Text where

import           Data.Foldable
import           Data.List        (intersperse)
import           Data.Semigroup
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format

class ToText a where
  toText :: a -> Text

  default toText :: Show a => a -> Text
  toText = T.pack . show

instance ToText Text where
  toText = id

instance ToText Char where
  toText = T.singleton

instance ToText String where
  toText = T.pack

instance ToText UTCTime where
  toText utc = T.pack $ formatTime defaultTimeLocale "%m/%d/%Y" utc

instance ToText Integer
instance ToText Int

instance (Foldable f, ToText a) => ToText (f a) where
  toText xs = "[" <> (foldr T.append "" $ intersperse ", " $ map toText $ toList xs) <> "]"
