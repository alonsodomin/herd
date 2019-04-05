{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Avro.Schema       as Avro
import qualified Data.Text              as T
import           System.Environment

import           Herd.Client
import           Herd.Types

main :: IO ()
main = do
  [host, port] <- getArgs
  runStderrLoggingT $ runHerdClient (T.pack host) (read port) $ herdProgram
  where herdProgram = do
          $(logDebug) "making some requests to the Herd server"

          let schema    = Avro.Boolean

          registerSchema "foo" schema
          subjectIds <- fetchSubjectIds

          liftIO $ print subjectIds
