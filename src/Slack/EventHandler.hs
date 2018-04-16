module Slack.EventHandler (
  handleSlackMessage
) where

import Web.Slack
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions
import qualified Network.IRC as IRC

import Slack.Utils
import Irc.Utils (formatNickName, buildJoinResponse)


formatPrivmsg :: Maybe Text -> Text -> Text -> IRC.Message
formatPrivmsg from to body = IRC.Message 
    (formatNickName `fmap` from) 
    "PRIVMSG" 
    [cs to, cs body]


handleSlackMessage :: (IRC.Message -> IO())
                   -> SlackSession 
                   -> Event 
                   -> IO ()

handleSlackMessage sendToIrc sess (Message cid from msg _ _ _ mAttachments) = do
    mapM_ (sendToIrc . (formatPrivmsg (Just fromName) toName)) bodies
    where bodies = (T.lines readableMessage) ++ fallbacks
          readableMessage = decodeSlackText sess msg
          fromName = lookupSubmitterName sess from
          toName = lookupChannelName sess cid
          fallbacks = case mAttachments of
                      Just attachments -> map attachmentFallback attachments
                      Nothing -> []

handleSlackMessage sendToIrc sess (ChannelJoined channel) = do 
    mapM_ sendToIrc $ buildJoinResponse sess channel

handleSlackMessage sendToIrc sess (UserTyping channelId userId) = do
    let channelName = channelNameFromId sess channelId
        userName    = usernameFromId sess userId
    sendToIrc $ formatPrivmsg (Just userName) channelName "\SOHACTION is typing\SOH"

handleSlackMessage sendToIrc sess event = do 
    sendToIrc $ IRC.Message Nothing "PRIVMSG" [(cs $ show event)]
