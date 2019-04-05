module Herd.Data.SchemaRegistry
     ( SchemaRegistry
     , empty
     , getSubjects
     , getVersions
     , getLatestVersion
     , getSchema
     , deleteSchema
     , registerSchema
     , size
     ) where

import           Data.Avro.Schema   (Schema)
import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map
import qualified Data.List.NonEmpty as NEL
import           Data.Map.NonEmpty  (NEMap)
import qualified Data.Map.NonEmpty  as NEM

import           Herd.Types

type SchemaRegistry = HashMap SubjectId (NEMap Version Schema)

latestVersion :: NEMap Version Schema -> Version
latestVersion = NEL.last . NEM.keys

empty :: SchemaRegistry
empty = Map.empty

getSubjects :: SchemaRegistry -> [SubjectId]
getSubjects = Map.keys

getVersions :: SubjectId -> SchemaRegistry -> Maybe [Version]
getVersions subjectId reg = (NEL.toList . NEM.keys) <$> Map.lookup subjectId reg

getLatestVersion :: SubjectId -> SchemaRegistry -> Maybe Version
getLatestVersion subjectId reg = latestVersion <$> Map.lookup subjectId reg

getSchema :: SubjectId -> Version -> SchemaRegistry -> Maybe Schema
getSchema subjectId version reg =
  let versions = Map.lookup subjectId reg
  in NEM.lookup version =<< versions

deleteSchema :: SubjectId -> Version -> SchemaRegistry -> SchemaRegistry
deleteSchema subjectId version =
  Map.update (NEM.nonEmptyMap . NEM.delete version) subjectId

registerSchema :: SubjectId -> Schema -> SchemaRegistry -> SchemaRegistry
registerSchema subjectId schema =
  Map.alter populateSchema subjectId
  where populateSchema :: Maybe (NEMap Version Schema) -> Maybe (NEMap Version Schema)
        populateSchema Nothing     = Just $ NEM.singleton initialVersion schema
        populateSchema (Just prev) = Just $
          let latestV = latestVersion prev
          in NEM.insert (nextVersion latestV) schema prev

size :: SchemaRegistry -> Integer
size = foldr (+) 0 . Map.elems . Map.map (fromIntegral . NEM.size)
