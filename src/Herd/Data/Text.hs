module Herd.Data.Text where

import           Data.Text (Text)
import qualified Data.Text as T

class ToText a where
  toText :: a -> Text

instance ToText Int where
  toText = T.pack . show
