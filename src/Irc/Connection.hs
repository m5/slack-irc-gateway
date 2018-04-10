{-# LANGUAGE QuasiQuotes                      #-}
{-# LANGUAGE OverloadedStrings                #-}

module Irc.Connection where

import Control.Concurrent
import Network
import System.IO
import Data.String.Conversions
import qualified Network.IRC as IRC
import qualified Data.ByteString.Char8 as B8
import Control.Monad (unless, forever, void, liftM)
import Control.Exception (try, IOException)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

-- data IrcHandle = IrcHandle {
--   handle :: Handle
--   remoteHost :: String
--   remotePort :: String
-- }


startIrc :: ((IRC.Message -> IO()) -> Either Text IRC.Message -> IO ()) 
         -> MVar [Handle] 
         -> PortNumber 
         -> IO ()
startIrc messageHandler clientsVar port = do
    sock <- listenOn $ PortNumber port
    forever $ do
        (h, remoteHost, remotePort) <- accept sock
        hSetBuffering h NoBuffering
        modifyMVar_ clientsVar $ return . (h:)
        forkIO $ do
            result <- try $ forever $ do
                line <- hGetLine h
                clients <- readMVar clientsVar
                let messageMaybe = IRC.decode $ B8.pack line
                    debugMessage = cs $ fromMaybe 
                      ("Couldn't decode message: " ++ line)
                      ((cs . IRC.showMessage) `fmap` messageMaybe)
                broadcastIrc clientsVar $ debugIrcMessage debugMessage
                messageHandler 
                    (sendIrcMessage clientsVar h) 
                    $ case messageMaybe of
                        Just message -> Right message
                        Nothing      -> Left $ cs line

            case result::(Either IOException ()) of
              Left _ -> do 
                removeHandler clientsVar h
              Right _ -> return ()


broadcastIrc :: MVar[Handle] -> IRC.Message -> IO ()
broadcastIrc clientsVar message = do
  clients <- readMVar clientsVar 
  mapM_ (\h -> sendIrcMessage clientsVar h message) clients

sendIrcMessage :: MVar[Handle] -> Handle -> IRC.Message -> IO ()
sendIrcMessage clientsVar handle message = do
  result <- try $ hPutStr handle $ (cs $ IRC.showMessage message) ++ "\r\n" 
         :: IO (Either IOException ())
  case result of
    Left _ -> removeHandler clientsVar handle
    Right _ -> return ()

removeHandler :: MVar [Handle] -> Handle -> IO ()
removeHandler clientsVar toRemove = do 
  modifyMVar_ clientsVar $ return . (filter (\h -> h /= toRemove))

debugIrcMessage :: String -> IRC.Message
debugIrcMessage message = IRC.Message 
  Nothing 
  "PRIVMSG" 
  ["Debug: ", cs message]
