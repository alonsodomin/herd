{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Avro.Schema       as Avro
import qualified Data.Avro.Types.Value  as AvroV
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NEL
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Environment

import           Herd.Client
import           Herd.Data.Text
import           Herd.Types             (AvroSchema (..))

main :: IO ()
main = do
  [host, port] <- getArgs
  runStderrLoggingT $ runClient (T.pack host) (read port) $ herdProgram
  where herdProgram = do
          $(logDebug) "making some requests to the Herd server"

          registerSchema "foo" $ AvroSchema Avro.Boolean
          registerSchema "bar" $ AvroSchema Avro.Int
          registerSchema "foo" $ AvroSchema $ Avro.mkUnion (Avro.Null :| [Avro.Boolean])
          registerSchema "quxx" $ AvroSchema $ Avro.Record
            { Avro.name = Avro.TN "quxx"
            , Avro.aliases = []
            , Avro.namespace = Nothing
            , Avro.doc = Nothing
            , Avro.order = Nothing
            , Avro.fields = [
                Avro.Field "field" [] Nothing Nothing Avro.Int (Just $ AvroV.Int 10)
              ]
            }

          subjectIds <- getSubjectIds
          liftIO . T.putStrLn $ "Subject IDs: " <> (toText subjectIds)

          versions <- getSchemaVersions "foo"
          liftIO . T.putStrLn $ "Versions: " <> (toText versions)

          schema <- getSchema "foo" (NEL.head versions)
          liftIO . putStrLn $ show schema

          recordId <- writeSubject "bar" (20 :: Int)
          liftIO . T.putStrLn $ toText recordId
