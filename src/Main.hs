module Main where

import Network
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread 
import System.Environment
import System.Exit
import System.IO
import Control.Monad (unless, forever, void, liftM)
import Web.Slack
import qualified Network.IRC as IRC

import Irc.Connection (startIrc, broadcastIrc)
import Irc.EventHandler (handleIrcMessage, formatNickName)
import Slack.EventHandler (handleSlackMessage)
import Config

data Args = Args {
    fileName :: String
}

main = withSocketsDo $ do
    (Args filename) <- getArgs >>= parseArgs
    configs <- getConfiguration filename
    forkResults <- mapM (startGateway) configs
    mapM_ id [waiter | (_, waiter) <- forkResults]

parseArgs :: [String] -> IO Args
parseArgs [] = do
  progname <- getProgName
  putStrLn $ "Usage: " ++ progname ++ " networksConfigFile"
  exitWith ExitSuccess
  
parseArgs [filename] = return $ Args filename

startGateway :: NetworkConfiguration -> IO (ThreadId, IO (Result ()))
startGateway (NetworkConfiguration port slackToken) = forkIO $ do
    let config = SlackConfig { _slackApiToken = slackToken }
    clientsVar <- newMVar []
    withSlackHandle config $ ircBot clientsVar port


ircBot ::  MVar [Handle] -> PortNumber -> SlackHandle -> IO ()
ircBot clientsRef port slackHandle = do 
    let sess = getSession slackHandle
        ircHandler = handleIrcMessage slackHandle 

    forkIO $ startIrc ircHandler clientsRef port

    forever $ do
        event <- getNextEvent slackHandle
        clients <- readMVar clientsRef
        handleSlackMessage (broadcastIrc clientsRef) sess event
