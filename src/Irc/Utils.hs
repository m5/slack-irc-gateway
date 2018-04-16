module Irc.Utils where

import Web.Slack
import qualified Network.IRC as IRC
import qualified Data.Text as T
import           Data.Text (Text)
import Data.List (intercalate)
import Data.String.Conversions
import Slack.Utils

buildWelcomeMessage :: SlackSession -> IRC.Message
buildWelcomeMessage sess = IRC.Message thisServerName "001" [username, "Welcome to Slack!"]
   where username = cs . _selfName . _slackSelf $ sess

buildWhoResponse :: SlackSession -> Channel -> [IRC.Message]
buildWhoResponse sess channel = [
    IRC.Message thisServerName "353" [userName, "=",  channelName, memberNames],
    IRC.Message thisServerName "366" [userName, cs channelName, "End of /NAMES list."]
  ]
    where userName = cs $ _selfName $ _slackSelf sess
          channelName = cs $ T.cons '#' (_channelName channel)
          memberNames = cs $ case _channelMembers channel of 
                        Just members -> intercalate " " $ map (T.unpack . usernameFromId sess) members
                        Nothing -> ""

buildJoinResponse :: SlackSession -> Channel -> [IRC.Message]
buildJoinResponse sess channel = [
    IRC.Message (Just (formatNickName userName)) "JOIN" [channelName],
    IRC.Message thisServerName "332" [channelName]
  ] ++ (buildWhoResponse sess channel)
    where userName = _selfName $ _slackSelf sess
          channelName = cs $ '#' `T.cons` (_channelName channel)
          topic = case _channelTopic channel of
                       Just t -> T.unpack $ _topicValue t
                       Nothing -> ""


formatNickName :: Text -> IRC.Prefix
formatNickName username = IRC.NickName (cs username) Nothing Nothing

thisServerName :: Maybe IRC.Prefix
thisServerName = Just $ IRC.Server "slack-gateway"

