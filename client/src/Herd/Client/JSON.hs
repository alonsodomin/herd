{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Herd.Client.JSON
     ( ClientT
     , runClient
     ) where

import           Conduit
import           Control.Lens           ((^?))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Conduit.Network   (clientSettings)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Network.JSONRPC

import           Herd.Client.Class
import           Herd.Protocol

newtype ClientT m a = ClientT { unwrapClient :: JSONRPCT m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadLogger)

instance MonadLoggerIO m => MonadClient (ClientT m) where
  sendToServer req l = ClientT $ do
    rawRes <- sendRequest req
    res    <- foldResponse rawRes

    case (res ^? l) of
      Nothing -> fail "invalid response"
      Just r  -> return r

    where
      foldResponse :: MonadIO m => Maybe (Either ErrorObj HerdResponse) -> m HerdResponse
      foldResponse Nothing          = fail "could not receive or parse response"
      foldResponse (Just (Left e))  = fail $ fromError e
      foldResponse (Just (Right r)) = return r

-- Run the Herd client monad

runClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> ClientT m a -> m a
runClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let cs = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True cs (unwrapClient action)
