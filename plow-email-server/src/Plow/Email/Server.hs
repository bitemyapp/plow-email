
{- |
Module      :  Plow.Email.Server
Description :  Plow.Email sends quick emails
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable (System.Posix)
-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Plow.Email.Server  where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy        (toStrict)

import           Control.Applicative         ((<$>))
import           Control.Exception           (SomeException, try)
import           Control.Monad               (void)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Lazy              (fromStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Network.HaskellNet.Auth     (AuthType (..))
import           Network.HaskellNet.SMTP     (Command (..), SMTPConnection,
                                              sendCommand, sendMail)
import           Network.HaskellNet.SMTP.SSL
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)
import           Network.Mail.Mime
import           Plow.Email.Lens             (eventEntries_, stateChangeMsg_,
                                              _EventStateChange)
import           Plow.Email.MailTemplate
import           Plow.Email.Types
import           Prelude                     hiding (concat)
import           System.IO                   (hPrint, stderr)

import           Yesod

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

data MailFoundation = MailFoundation

mkYesod "MailFoundation" [parseRoutes|
/ HomeR GET
/email EmailR POST
|]

instance Yesod MailFoundation

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Email
                                  Server|]


buildEmailSubject :: AlarmEmailTemplate -> Text
buildEmailSubject aet =  aetName aet <>  (" "::Text) <>  (pack . statusHumanReable $ aetStatus aet)

processMailList :: AlarmEmailTemplate -> Text -> IO Mail
processMailList aet email = do
  tz <- getCurrentTimeZone
  sMail <- simpleMail defaultFromAddress to' (buildEmailSubject aet)  "here is the plain body" (hamletToText $ alarmMailTemplate aet tz )  []
  return $ sMail {mailTo=to':[]}
    where
      to' = Address Nothing email

processAlarmEmailTemplate :: AlarmEmailTemplate -> [Text] -> IO [Mail]
processAlarmEmailTemplate aet  = traverse (processMailList aet)

decodeAR :: Text -> Maybe (AlarmRunner AnyAlarm AnyCall AnyCount)
decodeAR = decode.encodeUtf8.fromStrict

processAlarmRunner :: AlarmRunner AnyAlarm c ct -> SMTPConnection -> IO ()
processAlarmRunner ar connection = do
  let at' = fetchAlarmRunnerStuff ar
  let froms = mailPPL at'
  case froms of
      [] -> ePrint ("Email List is empty. Please check with system Administrator."::Text)
      _ -> do
           rms <- processAlarmEmailTemplate at' froms
           void $ liftIO $ sendCommand connection (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
           void $ traverse (\rm -> sendEmails rm froms connection) rms
           return ()

sendEmails :: Mail -> [Text] -> SMTPConnection -> IO ()
sendEmails rm froms connection = do
  email <- renderMail' rm
  sendMail  "alarms@plowtech.net" (unpack <$> froms)  (toStrict email) connection

postEmailR :: Handler Value
postEmailR = do
   var <- parseJsonBody :: Handler (Result [EventEntries])
   case var of
     (Error f) -> return . toJSON $ f
     (Success s) -> do
        connection <- liftIO $ connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
        let eventList = s ^.. (traverse . eventEntries_ .folded ) :: [AlarmLogEvent]
            msgTxt = eventList ^.. (traverse  . _EventStateChange . stateChangeMsg_ )
            ars = catMaybes $ decodeAR <$> msgTxt
            alarmRunnerCount = length ars
        rslt <- liftIO $ try $ traverse (`processAlarmRunner` connection) ars
        case rslt of
            Left (_e::SomeException) -> return . toJSON $ ("Error on sending email request."::Text)
            Right _ -> do
                  liftIO $ print ((show alarmRunnerCount ++ " AlarmRunners are process.")::String)
                  return . toJSON $ s



fetchAlarmRunnerStuff
  :: AlarmRunner AnyAlarm c ct -> AlarmEmailTemplate
fetchAlarmRunnerStuff (AlarmRunner { alarmTime = at'
                                   , alarmState = st
                                   , alarmParameters = ap, .. }) = AET at' (message ap) (alarm st) (getTo.getPeople.callList $  ap)
  where
    getTo = foldl fldFcn []
    fldFcn a b
     | emailAlert b = email b:a
     | otherwise =  a



