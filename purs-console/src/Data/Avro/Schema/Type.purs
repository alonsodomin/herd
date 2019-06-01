module Data.Avro.Schema where

import Data.Generic

data Type = Null

derive instance genericAvroType :: Generic Type