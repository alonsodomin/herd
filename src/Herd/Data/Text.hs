module Herd.Data.Text where

import           Data.Text (Text)

class ToText a where
  toText :: a -> Text
