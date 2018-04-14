module Slack.Utils where


import           Text.Regex.PCRE.Heavy
import           Data.Attoparsec.Text as P
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import qualified HTMLEntities.Decoder as HD
import Web.Slack
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)


normalizeChannels :: SlackSession -> [(ChannelId, Text)]
normalizeChannels sess = normalChannels ++ normalIms
    where normalChannels = map (\c -> (_channelId c, ('#' `T.cons` _channelName c))) $ channels
          channels = _slackChannels sess ++ _slackGroups sess
          normalIms = map (\c -> (imToChannel $ _imId c, usernameFromId sess (_imUser c))) $ _slackIms sess

channelNameFromId :: SlackSession -> ChannelId -> Text
channelNameFromId sess id = case matches of
                                  s:_ -> s
                                  []  -> T.pack "#unknown"
    where matches = map snd
                    . filter (\(i,n) -> id == i)
                    $ normalizeChannels sess

channelIdFromName :: SlackSession -> Text -> Maybe ChannelId
channelIdFromName sess name = case matches of
                                  s:_ -> Just s
                                  []  -> Nothing
    where matches = map fst
                    . filter (\(i,n) -> name == n)
                    $ normalizeChannels sess

lookupSubmitterName :: SlackSession -> Submitter -> Text
lookupSubmitterName sess (UserComment uid) = usernameFromId sess uid
lookupSubmitterName sess submitter = T.pack $ show submitter

lookupChannelName :: SlackSession -> ChannelId -> Text
lookupChannelName sess cid = channelNameFromId sess cid

decodeSlackText :: SlackSession -> Text -> Text
decodeSlackText sess slackText = entitiesDecoded
    where entitiesDecoded = toStrict . toLazyText . HD.htmlEncodedText $ tagsReplaced
          tagsReplaced = gsub [re|(<[^>]*>)|] replaceFn slackText
          replaceFn cap = decodeSlackTag sess cap

data SlackTag = SlackTag Char Text

slackTagParser :: P.Parser SlackTag
slackTagParser = do
    P.char '<'
    prefix <- P.anyChar
    id <- P.takeWhile (\c -> c /= '>')
    return $ SlackTag prefix id

decodeSlackTag :: SlackSession -> Text -> Text
decodeSlackTag sess tagText = case slackTag  of
         Right (SlackTag '@' userId)    -> '@' `T.cons` usernameFromId sess (Id userId)
         Right (SlackTag '#' channelId) -> '#' `T.cons` channelNameFromId sess (Id channelId)
         Right (SlackTag '!' notifName) -> '!' `T.cons` notifName
         Right (SlackTag  c  value)      ->  c `T.cons` value
         otherwise -> tagText
    where slackTag = parseOnly slackTagParser tagText

encodeSlackText :: SlackSession -> Text -> Text
encodeSlackText sess ircText = tagsReplaced
    where tagsReplaced = gsub [re|(@\w+)|] (encodeSlackUsername sess) ircText

encodeSlackUsername :: SlackSession -> Text -> Text
encodeSlackUsername sess atUsername = case maybeUserId of
        Just userId -> "<@" `T.append` userId `T.append` ">"
        Nothing -> atUsername
    where maybeUserId = idFromUsername sess (T.tail atUsername)

usernameFromId :: SlackSession -> UserId -> Text
usernameFromId sess id = case matches of 
                               u:_ -> bestUsername u
                               []  -> T.pack "unknown"
    where matches = filter (\u -> id == _userId u) users
          users   = _slackUsers sess
          
idFromUsername :: SlackSession -> Text -> Maybe Text
idFromUsername sess username = case matches of 
                               u:_ -> Just . _getId . _userId $ u
                               []  -> Nothing
    where matches = filter (\u -> username == bestUsername u) users
          users   = _slackUsers sess

bestUsername :: User -> Text
bestUsername user = fromMaybe username displayNameMaybe
  where displayNameMaybe = _profileDisplayName . _userProfile $ user
        username = _userName user

openChannels :: SlackSession -> [Channel]
openChannels sess = filter (\c -> _channelIsOpen c || _channelIsMember c) $ (_slackChannels sess ++ _slackGroups sess)

allChannels :: SlackSession -> [Channel]
allChannels sess = _slackChannels sess ++ _slackGroups sess
