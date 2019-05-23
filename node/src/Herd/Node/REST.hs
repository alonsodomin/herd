{-# LANGUAGE OverloadedStrings #-}

module Herd.Node.REST
     ( startRESTServer
     ) where

import Control.Monad.IO.Class
import Control.Lens
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import qualified Data.List.NonEmpty as NEL
import           Data.Avro.Schema            (Schema)

import Herd.Node.Config
import Herd.Node.API
import Herd.Node.Core
import Herd.Types

type HerdREST = "subjects" :> Get '[JSON] [SubjectId]
           :<|> "subjects" :> Capture "subjectId" SubjectId :> Get '[JSON] [Version]
           :<|> "subjects" :> Capture "subjectId" SubjectId :> Capture "version" Version :> Get '[JSON] (Maybe Schema)

instance FromHttpApiData SubjectId where
  parseUrlPiece = Right . SubjectId

instance FromHttpApiData Version where
  parseUrlPiece txt = do
    mv <- version <$> parseUrlPiece txt
    case mv of
      Just v  -> Right v
      Nothing -> Left $ "invalid version: " <> txt

herdREST :: Proxy HerdREST
herdREST = Proxy

herdRESTServer :: HerdEnv -> Server HerdREST
herdRESTServer env = handleGetSubjects env
                :<|> handleGetSubjectVersions env
                :<|> handleGetSchema env

herdRESTApp :: HerdEnv -> Application
herdRESTApp env = serve herdREST (herdRESTServer env)

-- Handlers

handleGetSubjects :: HerdEnv -> Handler [SubjectId]
handleGetSubjects env = liftIO $ runAction env $ invokeAction getSubjectIds

handleGetSubjectVersions :: HerdEnv -> SubjectId -> Handler [Version]
handleGetSubjectVersions env subjectId = do
  versions <- liftIO $ runAction env $ invokeAction (getSchemaVersions subjectId)
  case versions of
    Just vs -> return $ NEL.toList vs
    Nothing -> return []

handleGetSchema :: HerdEnv -> SubjectId -> Version -> Handler (Maybe Schema)
handleGetSchema env subjectId version =
  liftIO $ runAction env $ invokeAction (getSchema subjectId version)

-- Main server function

startRESTServer :: HerdConfig -> HerdEnv -> IO ()
startRESTServer cfg env = do
  let port = cfg ^. hcNetwork . ncHttp . nbPort
  run port (herdRESTApp env)