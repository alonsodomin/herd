module Herd.Console.Types where

import Prelude (class Eq, class Show, show, (<>), class Ord)

import Herd.Types (SubjectId(..), Version(..))

data SchemaId = SchemaId SubjectId Version

instance showSchemaId :: Show SchemaId where
  show (SchemaId (SubjectId subjectId) (Version version)) =
    subjectId <> "#" <> (show version)

derive instance eqSchemaId :: Eq SchemaId
derive instance ordSchemaId :: Ord SchemaId