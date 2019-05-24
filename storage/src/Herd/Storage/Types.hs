module Herd.Storage.Types where

import           Control.Lens
import           Data.String
import           Data.ByteString   (ByteString)
import           Data.Text    (Text)
import qualified Data.Text    as T

newtype BlockId = BlockId Text
  deriving (Eq, Show)

instance IsString BlockId where
  fromString = BlockId . T.pack

data Block =
    Genesis BlockId
  | Block BlockId BlockId ByteString
  deriving (Eq, Show)

blockId :: Getter Block BlockId
blockId = to getBlockId
  where getBlockId (Genesis bid)   = bid
        getBlockId (Block bid _ _) = bid

parentBlockId :: Getter Block (Maybe BlockId)
parentBlockId = to getParentBlockId
  where getParentBlockId (Genesis _)     = Nothing
        getParentBlockId (Block _ pid _) = Just pid
