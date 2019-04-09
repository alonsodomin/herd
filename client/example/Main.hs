{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Avro.Schema       as Avro
import qualified Data.List.NonEmpty     as NEL
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Environment

import           Herd.Client
import           Herd.Data.Text
import           Herd.Types

main :: IO ()
main = do
  [host, port] <- getArgs
  runStderrLoggingT $ runHerdClient (T.pack host) (read port) $ herdProgram
  where herdProgram = do
          $(logDebug) "making some requests to the Herd server"

          registerSchema "foo" Avro.Boolean
          registerSchema "bar" Avro.Int
          registerSchema "foo" Avro.String

          subjectIds <- getSubjectIds
          liftIO . T.putStrLn $ "Subject IDs: " <> (toText subjectIds)

          versions <- getSchemaVersions "foo"
          liftIO . T.putStrLn $ "Versions: " <> (toText versions)

          schema <- getSchema "foo" (NEL.head versions)
          liftIO . putStrLn $ show schema
