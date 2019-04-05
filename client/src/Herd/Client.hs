
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client
     ( fetchSubjectIds
     , runHerdClient
     ) where

import           Conduit
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
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

fetchSubjectIds :: MonadLoggerIO m => HerdClientT m [SubjectId]
fetchSubjectIds = do
  req <- sendRequest FetchSubjectIds
  res <- handleResponse req
  case res of
    FetchedSubjectIds subjectIds -> return subjectIds
    _                            -> fail "invalid response"

runHerdClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> HerdClientT m a -> m a
runHerdClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let jsonrcpSettings = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True jsonrcpSettings action
