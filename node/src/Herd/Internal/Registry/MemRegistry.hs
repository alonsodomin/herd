module Herd.Internal.Registry.MemRegistry
     ( RegistryState
     , MemRegistry
     , empty
     , initial
     , getSubjects
     , getVersions
     , getLatestVersion
     , getSchema
     , deleteSchema
     , registerSchema
     , size
     , runMemRegistry
     ) where

import           Control.Monad.State
import           Data.Avro.Schema    (Schema)
import           Data.Binary         (Binary)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as Map
import qualified Data.List.NonEmpty  as NEL
import           Data.Map.NonEmpty   (NEMap)
import qualified Data.Map.NonEmpty   as NEM
import           Data.Typeable
import           GHC.Generics

import           Herd.Internal.Types

type RegistryState = HashMap SubjectId (NEMap Version Schema)
type MemRegistry m = StateT RegistryState m

getSubjects :: Monad m => MemRegistry m [SubjectId]
getSubjects = do
  allSubjects <- get
  return $ Map.keys allSubjects

getVersions :: Monad m => SubjectId -> MemRegistry m (Maybe [Version])
getVersions subjectId = do
  allSubjects <- get
  return $ (NEL.toList . NEM.keys) <$> Map.lookup subjectId allSubjects

getLatestVersion :: Monad m => SubjectId -> MemRegistry m (Maybe Version)
getLatestVersion subjectId = do
  allSubjects <- get
  return $ (NEL.last . NEM.keys) <$> Map.lookup subjectId allSubjects

getSchema :: Monad m => SubjectId -> Version -> MemRegistry m (Maybe Schema)
getSchema subjectId version = do
  allSubjects <- get
  let versions = Map.lookup subjectId allSubjects
  return $ NEM.lookup version =<< versions

deleteSchema :: Monad m => SubjectId -> Version -> MemRegistry m ()
deleteSchema subjectId version =
  modify (Map.update (NEM.nonEmptyMap . NEM.delete version) subjectId)

registerSchema :: Monad m => SubjectId -> Schema -> MemRegistry m ()
registerSchema subjectId schema =
  modify (Map.alter populateSchema subjectId)
  where populateSchema :: Maybe (NEMap Version Schema) -> Maybe (NEMap Version Schema)
        populateSchema Nothing     = Just $ NEM.singleton initialVersion schema
        populateSchema (Just prev) = Just $
          let latestV = NEL.head . NEL.sort $ NEM.keys prev
          in NEM.insert (nextVersion latestV) schema prev

size :: Monad m => MemRegistry m Integer
size = do
  allSubjects <- get
  return $ foldr (+) 0 $ Map.elems $ Map.map (fromIntegral . NEM.size) allSubjects

empty :: RegistryState
empty = Map.empty
{-# INLINE empty #-}

initial = empty
{-# DEPRECATED initial "Use 'empty' instead" #-}
{-# INLINE initial #-}

runMemRegistry :: Monad m => MemRegistry m a -> m a
runMemRegistry registry = evalStateT registry initial
