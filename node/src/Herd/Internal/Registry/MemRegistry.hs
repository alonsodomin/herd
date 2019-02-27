{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Herd.Internal.Registry.MemRegistry
     ( RegistryState
     , initial
     , MemRegistry
     , getSubjects
     , getVersions
     , getSchema
     , deleteSchema
     , publishSchema
     , runMemRegistry
     ) where

import           Control.Monad.State
import           Data.Avro.Schema
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

getSchema :: Monad m => SubjectId -> Version -> MemRegistry m (Maybe Schema)
getSchema subjectId version = do
  allSubjects <- get
  let versions = Map.lookup subjectId allSubjects
  return $ NEM.lookup version =<< versions

deleteSchema :: Monad m => SubjectId -> Version -> MemRegistry m ()
deleteSchema subjectId version =
  modify (Map.update (NEM.nonEmptyMap . NEM.delete version) subjectId)

publishSchema :: Monad m => SubjectId -> Schema -> MemRegistry m ()
publishSchema subjectId schema =
  modify (Map.alter populateSchema subjectId)
  where populateSchema :: Maybe (NEMap Version Schema) -> Maybe (NEMap Version Schema)
        populateSchema Nothing     = Just $ NEM.singleton firstVersion schema
        populateSchema (Just prev) = Just $
          let latestV = NEL.head . NEL.sort $ NEM.keys prev
          in NEM.insert (nextVersion latestV) schema prev

initial :: RegistryState
initial = Map.empty

runMemRegistry :: Monad m => MemRegistry m a -> m a
runMemRegistry registry = evalStateT registry initial
