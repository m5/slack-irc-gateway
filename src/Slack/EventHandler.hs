module Slack.EventHandler (
  handleSlackMessage
) where

import Web.Slack
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions
import qualified Network.IRC as IRC

import Slack.Utils
import Irc.EventHandler (formatNickName)

formatSlackMessage :: Event -> SlackSession -> [IRC.Message]
formatSlackMessage (Message cid from msg _ _ _ mAttachments) sess = 
    map (formatPrivmsg (Just fromName) toName) bodies
    where bodies = (T.lines readableMessage) ++ fallbacks
          readableMessage = decodeSlackText sess msg
          fromName = lookupSubmitterName sess from
          toName = lookupChannelName sess cid
          fallbacks = case mAttachments of
                      Just attachments -> map attachmentFallback attachments
                      Nothing -> []
formatSlackMessage ev sess = [IRC.Message Nothing "PRIVMSG" [(cs $ show ev)]]

formatPrivmsg :: Maybe Text -> Text -> Text -> IRC.Message
formatPrivmsg from to body = IRC.Message 
    (formatNickName `fmap` from) 
    "PRIVMSG" 
    [cs to, cs body]


handleSlackMessage :: (IRC.Message -> IO())
                   -> SlackSession 
                   -> Event 
                   -> IO ()
handleSlackMessage sendToIrc sess event = mapM_ sendToIrc $ formatSlackMessage event sess
