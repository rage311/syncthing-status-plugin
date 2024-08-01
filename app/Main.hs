{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (main) where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Network.HTTP.Req
import qualified Data.ByteString as BS
import Options.Applicative hiding (header)

  
data SyncthingDbStatus = SyncthingDbStatus {
  globalBytes                   :: !Int,
  globalDeleted                 :: !Int,
  globalDirectories             :: !Int,
  globalFiles                   :: !Int,
  globalSymlinks                :: !Int,
  globalTotalItems              :: !Int,
  ignorePatterns                :: !Bool,
  inSyncBytes                   :: !Int,
  inSyncFiles                   :: !Int,
  invalid                       :: !String,
  localBytes                    :: !Int,
  localDeleted                  :: !Int,
  localDirectories              :: !Int,
  localFiles                    :: !Int,
  localSymlinks                 :: !Int,
  localTotalItems               :: !Int,
  needBytes                     :: !Int,
  needDeletes                   :: !Int,
  needDirectories               :: !Int,
  needFiles                     :: !Int,
  needSymlinks                  :: !Int,
  needTotalItems                :: !Int,
  pullErrors                    :: !Int,
  receiveOnlyChangedBytes       :: !Int,
  receiveOnlyChangedDeletes     :: !Int,
  receiveOnlyChangedDirectories :: !Int,
  receiveOnlyChangedFiles       :: !Int,
  receiveOnlyChangedSymlinks    :: !Int,
  receiveOnlyTotalItems         :: !Int,
  sequence                      :: !Int,
  state                         :: !String,
  -- timestamp
  stateChanged                  :: !String,
  version                       :: !Int
} deriving (Generic, Show)

instance ToJSON SyncthingDbStatus
instance FromJSON SyncthingDbStatus

data Config = Config {
  apiKey :: !BS.ByteString,
  host   :: !Text,
  secure :: !Bool,
  port'  :: !Int,
  folder :: !Text,
  debug  :: !Bool
} deriving (Show)

config :: Parser Config
config = Config <$>
  strOption (
    long "api-key"
    <> short 'a'
    <> metavar "APIKEY"
    <> help "Syncthing API key"
  ) <*> strOption (
    long "host"
    <> short 'h'
    <> metavar "HOST"
    <> help "Hostname/IP"
    <> showDefault
    <> value "127.0.0.1"
  ) <*> flag False True (
    long "secure"
    <> short 's'
    <> help "Whether to use TLS via HTTPS"
  ) <*> option auto (
    long "port"
    <> short 'p'
    <> help "Host TCP port"
    <> showDefault
    <> value 8384
    <> metavar "PORT"
  ) <*> strOption (
    long "folder"
    <> short 'f'
    <> metavar "FOLDER"
    <> help "Folder name"
    <> showDefault
    <> value "default"
  ) <*> flag False True (
    long "debug"
    <> short 'd'
    <> help "More verbose output"
  )

main :: IO ()
main = getStatus =<< execParser opts
  where
    opts = info
      (config <**> helper)
      (fullDesc <> progDesc "Retrieve Syncthing DB status")

debugPrint :: (Show a) => Bool -> a -> IO ()
debugPrint True msg = print msg
debugPrint _ _ = return ()

getStatus :: Config -> IO ()
getStatus cfg@(Config { .. }) = runReq defaultHttpConfig $ do
  liftIO $ debugPrint debug (apiKey :: BS.ByteString)
  liftIO $ debugPrint debug cfg

  let options =
        port port'
        <> header "X-API-Key" apiKey
        <> "folder" =: folder

  r <-
    if secure then
      req
        GET (https host /: "rest" /: "db" /: "status")
        NoReqBody
        jsonResponse
        options
    else
      req
        GET (http host /: "rest" /: "db" /: "status")
        NoReqBody
        jsonResponse
        options
    
  let resp = (responseBody r :: SyncthingDbStatus)
  liftIO $ debugPrint debug resp

  liftIO $ putStrLn
    $ state resp
      ++ ": " ++ show (needTotalItems resp) ++ " items"
      ++ " (" ++ show (needBytes resp) ++ "B)"

  return ()
