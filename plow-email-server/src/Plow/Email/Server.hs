
{- |
Module      :  Plow.Email.Server
Description :  Plow.Email sends quick emails
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable (System.Posix)
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Plow.Email.Server  (startPlowEmail
                          ) where


import           Control.Exception         (finally)
import           Control.Monad             (void)
import           Data.Text                 (Text)
import qualified Filesystem.Path.CurrentOS as OS (FilePath, fromText)
import           Network.Wai.Handler.Warp  (defaultSettings, runSettings,
                                            setHost, setPort, setTimeout)
import           Plow.Email.CMD            (getOpts)
import           Plow.Email.Config         (PlowEmailConfig (..),
                                            getHostPreference,
                                            readNodeManagerConf,
                                            readPlowEmailConf)
import           Plow.Email.Handler        (MailFoundation (..))
import           System.Environment        (getArgs, withArgs)
import           System.IO                 (hPrint, stderr)
import           Yesod.Core.Dispatch       (toWaiApp)

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

defaultPlowEmailConfPath :: OS.FilePath
defaultPlowEmailConfPath = OS.fromText ("plow-email-config.yml"::Text)

defaultNodeManagerConfPath :: OS.FilePath
defaultNodeManagerConfPath = OS.fromText ("node-manager-config.yml"::Text)

startPlowEmail :: IO ()
startPlowEmail = do
  args <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- withArgs args getOpts
  print opts
  nmcfg <- readNodeManagerConf defaultNodeManagerConfPath
  pecfg <- readPlowEmailConf nmcfg defaultPlowEmailConfPath opts
  finally (
         print ("Starting ..."::Text) >> startServer pecfg MailFoundation
           ) (void $ do
                 let msg :: Text
                     msg = "Closing Node Manager Server"
                 ePrint msg)

startServer :: PlowEmailConfig -> MailFoundation -> IO ()
startServer pecfg peFoundation =  do
  app <- toWaiApp peFoundation
  let plowEmailDefaults = setTimeout (3*60).
                            (setHost.getHostPreference.plowEmailHost $ pecfg).
                              (setPort. plowEmailPort $ pecfg) $ defaultSettings
  runSettings plowEmailDefaults app

