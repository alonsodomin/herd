{-# LANGUAGE OverloadedStrings #-}

module Herd.HTTP.Registry where

import           Control.Monad.IO.Class (liftIO)
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString.Lazy   as BS
import           Data.Semigroup
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Text.Extra
import           Servant
import           Transient.Move

import           Herd.Core
import           Herd.Internal.Types

fetchSubjects :: Node -> Handler [SubjectId]
fetchSubjects = liftIO . getSubjects

fetchVersions :: Node -> SubjectId -> Handler [Version]
fetchVersions node subjectId = do
  foundSubject <- liftIO $ getVersions node subjectId
  case foundSubject of
      Just vs -> return vs
      Nothing -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Subject '" <> (toText subjectId) <> "' not found." }

fetchSchema :: Node -> SubjectId -> Version -> Handler Schema
fetchSchema node sid v = do
  foundSchema <- liftIO $ getSchema node sid v
  case foundSchema of
    Just sch -> return sch
    Nothing  -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Schema for subject '" <> (toText sid) <> "' with version " <> (toText v) <> " not found." }
