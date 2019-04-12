{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Herd.Client.Stub
     ( ClientStubT
     , whenRequest
     , replyWith
     , runClientStubT
     ) where

import           Control.Lens           ((^?))
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Hashable          (Hashable)
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as Map

import           Herd.Client.Class
import           Herd.Protocol

data StubbedOp = StubbedOp HerdRequest HerdResponse

newtype ClientStubT m a = ClientStubT
  { stubState :: StateT (HashMap String HerdResponse) m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

whenRequest :: Monad m => StubbedOp -> ClientStubT m ()
whenRequest (StubbedOp req res) = ClientStubT $ do
  modify (Map.insert (show req) res)

replyWith :: HerdRequest -> HerdResponse -> StubbedOp
replyWith = StubbedOp

instance Monad m => MonadClient (ClientStubT m) where
  sendToServer req l = ClientStubT $ do
    reqMap  <- get
    stubRes <- pure $ (Map.lookup (show req) reqMap) >>= (\x -> x ^? l)
    case stubRes of
      Nothing -> fail $ "Unexpected request: " ++ (show req)
      Just x  -> return x

runClientStubT :: Monad m => ClientStubT m a -> m a
runClientStubT stub = evalStateT (stubState stub) Map.empty
