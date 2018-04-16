module Irc.EventHandler (
  handleIrcMessage,
  formatNickName
) where

import Web.Slack
import Data.Text (Text)
import Data.String.Conversions
import qualified Data.Text  as T
import qualified Network.IRC as IRC
import qualified Data.ByteString as BS
import Slack.Utils
import Irc.Utils
import Irc.Connection (debugIrcMessage)
import Data.List (intercalate)
import Control.Lens
import Control.Monad.Trans.Except (runExceptT)

handleIrcMessage :: SlackHandle -> (IRC.Message -> IO()) -> Either Text IRC.Message -> IO ()
handleIrcMessage slack sendIrcMessage (Right message@(IRC.Message prefix cmd params)) = do
    let handler = case cmd of
                   "PRIVMSG" -> handlePrivmsg
                   "NICK" -> handleNick
                   "PING" -> handlePing
                   "WHO" -> handleWho
                   "JOIN" -> handleJoin
                   otherwise -> handleUnsupported
    handler slack sendIrcMessage message

handlePrivmsg :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handlePrivmsg slack sendIrcMessage (IRC.Message prefix cmd params) = do
    let (channel:message:[]) = params
        sess = getSession slack
    let mcid = channelIdFromName sess (cs channel) 
    case mcid of
       Just cid -> sendMessage slack cid $ encodeSlackText sess (cs message)
       Nothing -> sendIrcMessage $ debugIrcMessage $ "Could not send message, unknown channel: " ++ (cs channel)

handlePing :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handlePing slack sendIrcMessage (IRC.Message prefix cmd params) = do
    sendIrcMessage $ IRC.Message thisServerName "PONG" params

handleNick :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handleNick slack sendIrcMessage (IRC.Message prefix cmd params) = do
    let sess = getSession slack
        joinMessages = foldl (++) [] $ map (buildJoinResponse sess) $ openChannels sess
        welcomeMessage = buildWelcomeMessage sess
    sendIrcMessage welcomeMessage
    mapM_ sendIrcMessage joinMessages

handleWho :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handleWho slack sendIrcMessage (IRC.Message prefix cmd params) = do
    let (namePart:rest) = params
        '#':name = cs namePart
        sess = getSession slack
        channels = filter (\c -> (cs name) == view channelName c) 
                 . allChannels 
                 $ sess
        whoResponses = map (buildWhoResponse sess) channels
    case whoResponses of
         who:_ -> mapM_ sendIrcMessage who
         [] -> sendIrcMessage $ debugIrcMessage ("No channel found: " ++ (cs name))

handleJoin :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handleJoin slack sendIrcMessage (IRC.Message prefix cmd params) = do
    let channels = map (T.tail . cs) params
    runExceptT $ mapM_ (joinChannel slack) channels
    return ()

handleUnsupported :: SlackHandle -> (IRC.Message -> IO()) -> IRC.Message -> IO ()
handleUnsupported slack sendIrcMessage message = do
    sendIrcMessage $ debugIrcMessage $ "Unsupported command: " ++ (cs $ IRC.showMessage message)
