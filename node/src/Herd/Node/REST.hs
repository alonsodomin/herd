{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Herd.Node.REST
     ( HerdREST
     , herdREST
     , startRESTServer
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.List.NonEmpty          as NEL
import           Data.Maybe                  (isJust)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

import           Herd.Node.API
import           Herd.Node.Config
import           Herd.Node.Core
import           Herd.Types

type HerdREST = "subjects" :> Get '[JSON] [SubjectId]
           :<|> "subjects" :> Capture "subjectId" SubjectId :> Get '[JSON] [Version]
           :<|> "subjects" :> Capture "subjectId" SubjectId :> Capture "version" Version :> Get '[JSON] (Maybe AvroSchema)
           :<|> "subjects" :> Capture "subjectId" SubjectId :> ReqBody '[JSON] AvroSchema :> Post '[JSON] ()
           :<|> "subjects" :> Capture "subjectId" SubjectId :> Capture "version" Version :> Delete '[JSON] ()

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
                :<|> handleRegisterSchema env
                :<|> handleDeleteSchema env

herdRESTApp :: HerdEnv -> Application
herdRESTApp env = simpleCors $ serve herdREST (herdRESTServer env)

-- Handlers

handleGetSubjects :: HerdEnv -> Handler [SubjectId]
handleGetSubjects env = liftIO $ runAction env $ invokeAction getSubjectIds

handleGetSubjectVersions :: HerdEnv -> SubjectId -> Handler [Version]
handleGetSubjectVersions env subjectId = do
  versions <- liftIO $ runAction env $ invokeAction (getSchemaVersions subjectId)
  case versions of
    Just vs -> return $ NEL.toList vs
    Nothing -> return []

handleGetSchema :: HerdEnv -> SubjectId -> Version -> Handler (Maybe AvroSchema)
handleGetSchema env subjectId version =
  liftIO $ runAction env $ invokeAction (getSchema subjectId version)

handleRegisterSchema :: HerdEnv -> SubjectId -> AvroSchema -> Handler ()
handleRegisterSchema env subjectId schema =
  liftIO $ runAction env $ invokeAction (registerSchema subjectId schema)

handleDeleteSchema :: HerdEnv -> SubjectId -> Version -> Handler ()
handleDeleteSchema env subjectId version = do
  deleted <- liftIO $ runAction env $ invokeAction (deleteSchema subjectId version)
  if (isJust deleted) then return ()
  else throwError $ err404 { errBody = "Schema not found" }

-- Main server function

startRESTServer :: HerdConfig -> HerdEnv -> IO ()
startRESTServer cfg env = do
  let port = cfg ^. hcNetwork . ncHttp . nbPort
  run port (herdRESTApp env)
