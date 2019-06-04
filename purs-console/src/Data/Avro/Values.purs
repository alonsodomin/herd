module Data.Avro.Values
  ( Value(..)
  , genValue
  ) where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen, chooseBool, chooseFloat, chooseInt)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))

data Value t =
    Null
  | Boolean Boolean
  | Int Int
  | Long Int
  | Float Number
  | Double Number
  | Bytes ByteString
  | String String
  | Array (Array (Value t))
  | Map (Map String (Value t))
  | Record t (Map String (Value t))
  | Union (NonEmptyList t) t (Value t)
  | Fixed t ByteString
  | Enum t String

derive instance genericAvroValue :: Generic (Value t) _

genValue :: forall m t. MonadGen m => MonadRec m => Lazy (m (Value t)) => t -> m (Value t)
genValue typ = genLeaf
  where genLeaf :: m (Value t)
        genLeaf = Gen.oneOf $ pure Null :| [
            genBoolean
          , genInteger Int
          , genInteger Long
          , genNumber Float
          , genNumber Double
          , genBytes
          , genString
          , genArray
          , genMap
          , genRecord
          ]

        genBoolean :: m (Value t)
        genBoolean = Boolean <$> chooseBool

        genInteger :: (Int -> Value t) -> m (Value t)
        genInteger f = f <$> chooseInt top bottom

        genNumber :: (Number -> Value t) -> m (Value t)
        genNumber f = f <$> chooseFloat top bottom

        genBytes :: m (Value t)
        genBytes = Bytes <$> BS.toUTF8 <$> genUnicodeString

        genString :: m (Value t)
        genString = String <$> genUnicodeString

        genArray :: m (Value t)
        genArray = Array <$> Gen.unfoldable (defer \_ -> genValue typ)

        genValueMap :: m (Map String (Value t))
        genValueMap = do
          values <- (Gen.unfoldable (Tuple <$> genUnicodeString <*> (defer \_ -> genValue typ))) :: m (Array (Tuple String (Value t)))
          pure $ Map.fromFoldable values

        genMap :: m (Value t)
        genMap = Map <$> genValueMap

        genRecord :: m (Value t)
        genRecord = (Record typ) <$> genValueMap