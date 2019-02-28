{-# LANGUAGE OverloadedStrings #-}

module Herd.HTTP.Registry
     ( httpRegistry
     ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString.Lazy   as BS
import           Data.Semigroup
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Text.Extra
import           Servant
import           Transient.Move

import           Herd.Core
import           Herd.HTTP.API
import           Herd.Internal.Types

fetchSubjects :: Node -> Handler [SubjectId]
fetchSubjects = liftIO . (runDispatch getSubjects)

fetchVersions :: Node -> SubjectId -> Handler [Version]
fetchVersions node subjectId = do
  foundSubject <- liftIO $ runDispatch (getVersions subjectId) node
  case foundSubject of
      Just vs -> return vs
      Nothing -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Subject '" <> (toText subjectId) <> "' not found." }

fetchSchema :: Node -> SubjectId -> Version -> Handler Schema
fetchSchema node sid v = do
  foundSchema <- liftIO $ runDispatch (getSchema sid v) node
  case foundSchema of
    Just sch -> return sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' with version " <> (toText v) <> " not found." }

deleteSchema' :: Node -> SubjectId -> Version -> Handler Schema
deleteSchema' node sid v = do
  foundSchema <- liftIO $ runDispatch (deleteSchema sid v) node
  case foundSchema of
    Just sch -> return sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' with version " <> (toText v) <> " not found." }

registerSchema' :: Node -> SubjectId -> Schema -> Handler Version
registerSchema' node sid sch = do
  foundVersion <- liftIO $ runDispatch (registerSchema sid sch) node
  case foundVersion of
    Just v -> return v
    Nothing -> throwError $ err400 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' could not be registered." }

httpRegistry :: Node -> Server RegistryAPI
httpRegistry node = (fetchSubjects node)
               :<|> (fetchVersions node)
               :<|> (fetchSchema node)
               :<|> (registerSchema' node)
               :<|> (deleteSchema' node)

