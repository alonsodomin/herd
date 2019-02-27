{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Herd.Internal.Registry.MemRegistry
     ( RegistryState
     , initial
     , MemRegistry
     , getSubjects
     , getVersions
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
  versions    <- pure $ Map.lookup subjectId allSubjects
  return $ NEM.lookup version =<< versions

initial :: RegistryState
initial = Map.empty

runMemRegistry :: Monad m => MemRegistry m a -> m a
runMemRegistry registry = evalStateT registry initial
