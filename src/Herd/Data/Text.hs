{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Data.Text where

import           Data.Text (Text)
import qualified Data.Text as T

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
