
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client
     ( getSubjectIds
     , getSchemaVersions
     , registerSchema
     , runHerdClient
     ) where

import           Conduit
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString        as BS
import           Data.Conduit.Network   (clientSettings)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Network.JSONRPC

import           Herd.Protocol
import           Herd.Types

-- data ClientSettings = ClientSettings
--   { _csHost :: Text
--   , _csPort :: Int
--   } deriving (Eq, Show)

-- makeLenses ''ClientSettings

type HerdClientT m = JSONRPCT m

handleResponse :: MonadIO m => Maybe (Either ErrorObj HerdResponse) -> m HerdResponse
handleResponse Nothing          = fail "could not receive or parse response"
handleResponse (Just (Left e))  = fail $ fromError e
handleResponse (Just (Right r)) = return r

getSubjectIds :: MonadLoggerIO m => HerdClientT m [SubjectId]
getSubjectIds = do
  req <- sendRequest GetSubjectIds
  res <- handleResponse req
  case res of
    SubjectIds subjectIds -> return subjectIds
    _                     -> fail "invalid response"

getSchemaVersions :: MonadLoggerIO m => SubjectId -> HerdClientT m [Version]
getSchemaVersions subjectId = do
  req <- sendRequest $ GetSchemaVersions subjectId
  res <- handleResponse req
  case res of
    SchemaVersions versions -> return versions
    _                       -> fail "invalid response"

registerSchema :: MonadLoggerIO m => SubjectId -> Schema -> HerdClientT m ()
registerSchema subjectId schema = do
  req <- sendRequest $ RegisterSchema subjectId schema
  res <- handleResponse req
  case res of
    Done -> return ()
    _    -> fail "invalid response"

runHerdClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> HerdClientT m a -> m a
runHerdClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let cs = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True cs action
