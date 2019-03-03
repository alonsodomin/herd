{-# LANGUAGE OverloadedStrings #-}

module Herd.HTTP.Registry
     ( httpRegistry
     ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString.Lazy   as BS
import           Data.Semigroup
import           Data.Text.Encoding     (encodeUtf8)
import           Servant
import           Transient.Move

import           Herd.Core
import           Herd.Data.Text
import           Herd.HTTP.API
import           Herd.Internal.Types

getSubjects' :: Node -> Handler [SubjectId]
getSubjects' = liftIO . (runDispatch getSubjects)

getVersions' :: Node -> SubjectId -> Handler [Version]
getVersions' node subjectId = do
  foundSubject <- liftIO $ runDispatch (getVersions subjectId) node
  case foundSubject of
      Just vs -> return vs
      Nothing -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Subject '" <> (toText subjectId) <> "' not found." }

getSchema' :: Node -> SubjectId -> Version -> Handler Schema
getSchema' node sid v = do
  foundSchema <- liftIO $ runDispatch (getSchema sid v) node
  case foundSchema of
    Just sch -> return $ unwrapSchema sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' with version " <> (toText v) <> " not found." }

getLatestSchema' :: Node -> SubjectId -> Handler Schema
getLatestSchema' node sid = do
  foundSchema <- liftIO $ runDispatch (getLatestSchema sid) node
  case foundSchema of
    Just sch -> return $ unwrapSchema sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "No schema for subject '" <> (toText sid) <> "' could be found." }

deleteSchema' :: Node -> SubjectId -> Version -> Handler Schema
deleteSchema' node sid v = do
  foundSchema <- liftIO $ runDispatch (deleteSchema sid v) node
  case foundSchema of
    Just sch -> return $ unwrapSchema sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' with version " <> (toText v) <> " not found." }

registerSchema' :: Node -> SubjectId -> Schema -> Handler Version
registerSchema' node sid sch = do
  foundVersion <- liftIO $ runDispatch (registerSchema sid $ AvroSchema sch) node
  case foundVersion of
    Just v -> return v
    Nothing -> throwError $ err400 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' could not be registered." }

httpRegistry :: Node -> Server RegistryAPI
httpRegistry node = (getSubjects' node)
               :<|> (getVersions' node)
               :<|> (getSchema' node)
               :<|> (getLatestSchema' node)
               :<|> (registerSchema' node)
               :<|> (deleteSchema' node)

