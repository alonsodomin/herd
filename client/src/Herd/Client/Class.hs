module Herd.Client.Class where

import           Control.Lens  (Getting)
import           Data.Monoid   (First)

import           Herd.Protocol

class Monad m => MonadClient m where
  sendToServer :: HerdRequest -> Getting (First a) HerdResponse a -> m a
