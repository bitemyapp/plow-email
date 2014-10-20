{- |
Module      :  Plow.Email.MailClient
Description :  Email Sending Function
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable (System.Posix)
-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Plow.Email.MailClient (   defaultMailClient
                               , getConnection
                               , sendEmails
                               , authenticateMailClient
                               , SMTPConnection
                             ) where

import           Control.Applicative         ((<$>))
import           Data.Text                   (Text, unpack)

import           Data.ByteString.Lazy        (toStrict)
import           Network.HaskellNet.Auth     (AuthType (..))
import           Network.HaskellNet.SMTP     (Command (..), SMTPConnection,
                                              sendCommand, sendMail)
import           Network.HaskellNet.SMTP.SSL
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)
import           Network.Mail.Mime
import           Prelude

defaultMailClient :: String
defaultMailClient = "smtp.gmail.com"

getConnection ::IO SMTPConnection
getConnection = connectSMTPSSLWithSettings defaultMailClient (defaultSettingsWithPort 465)


authenticateMailClient connection = sendCommand connection (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")

sendEmails :: Mail -> [Text] -> SMTPConnection -> IO ()
sendEmails rm froms connection = do
  email <- renderMail' rm
  sendMail  "alarms@plowtech.net" (unpack <$> froms)  (toStrict email) connection
