module Herd.Console.Schema.List where

import Data.Map (Map)

import Herd.Types (SubjectId, Version)

newtype RawSchemaList =
  SchemaList (Map SubjectId Version)