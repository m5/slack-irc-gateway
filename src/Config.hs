module Config where

import Control.Monad.Except
import Data.Either
import Data.Ini
import Network (PortNumber)
import Data.Text (Text)
import Data.String.Conversions

data NetworkConfiguration = NetworkConfiguration {
  port :: PortNumber,
  token :: String
} deriving (Show)

getConfiguration :: String -> IO [NetworkConfiguration]
getConfiguration fileName = do
  configData <- cs <$> readFile fileName
  let configs = fromRight [] $ parseConfigurations configData
  return configs

parseConfigurations :: Text -> Either String [NetworkConfiguration]
parseConfigurations configText = do 
    ini <- parseIni configText
    let names = sections ini
    configs <- mapM (parseConfiguration ini) names
    return configs

parseConfiguration :: Ini -> Text -> Either String NetworkConfiguration
parseConfiguration ini section = do
    port <- lookupValue section "port" ini
    token <- lookupValue section "token" ini
    return $ NetworkConfiguration (read $ cs port) (cs token)
