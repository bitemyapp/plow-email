{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plow.Email.Config ( PlowEmailConfig (..)
                         , NodeManagerConfig (..)
                         , MyHostPreference (..)
                         , buildRequestObj
                         , readPlowEmailConf
                         , readNodeManagerConf
                       ) where

-- General
import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad             (mzero)
import           Data.Aeson
import           Data.Text                 (Text, unpack)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
-- File Reading
import qualified Filesystem.Path.CurrentOS as OS (FilePath, toText)
-- Node Config
import           Node.Client.Configs       (MyHostPreference (..),
                                            NodeManagerConfig (..),
                                            buildConfigName, readConfigFile,
                                            readNodeManagerConf)
import           Plow.Email.CMD            (MyOptions (..))

-- Config file for FilePaht, host and port Number
data PlowEmailConfig = PlowEmailConfig {
      mailFilePath  :: Text
    , plowEmailHost :: MyHostPreference
    , plowEmailPort :: Int
    } deriving (Read, Eq, Show, Typeable,Generic)

instance FromJSON PlowEmailConfig where
         parseJSON (Object o) = PlowEmailConfig <$>
                        ((o .: "plow-email-config") >>= (.: "mailFilePath"))
                    <*> ((o .: "plow-email-config") >>= (.: "plowEmailHost"))
                    <*> ((o .: "plow-email-config") >>= (.: "plowEmailPort"))
         parseJSON _ = mzero

instance ToJSON PlowEmailConfig where
  toJSON (PlowEmailConfig l h p) = object ["plow-email-config" .= b]
                  where b = object ["mailFilePath" .= l, "plowEmailHost" .= h, "plowEmailPort" .=p]

buildRequestObj :: MyOptions -> String -> Value
buildRequestObj opts cfgName =
  case port opts of
   Nothing -> object ["configName" .= cfgName]
   Just pNumber -> object ["configName" .= cfgName , "rewrite-rules" .= object ["key" .= ("alarmLogPort" ::String) , "val".= pNumber]]

-- Read Config Function
readPlowEmailConf :: NodeManagerConfig -> OS.FilePath -> MyOptions -> IO PlowEmailConfig
readPlowEmailConf nmcfg fpath opts =
        case OS.toText fpath of
         Left  e -> fail (unpack e)
         Right filepathText -> do
            let requestObj = buildRequestObj opts (buildConfigName filepathText)
            readConfigFile nmcfg fpath requestObj :: IO PlowEmailConfig
