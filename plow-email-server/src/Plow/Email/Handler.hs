
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
module Plow.Email.Handler  where

import           Control.Applicative     ((<$>))
import           Control.Exception       (SomeException, try)
import           Control.Lens
import           Control.Monad           (void)
import           Data.Aeson
import           Data.Maybe              (catMaybes)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           Data.Text.Lazy          (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Mail.Mime
import           Plow.Email.Lens         (eventEntries_, stateChangeMsg_,
                                          _EventStateChange)
import           Plow.Email.MailClient
import           Plow.Email.MailTemplate
import           Plow.Email.Types
import           Prelude                 hiding (concat)
import           System.IO               (hPrint, stderr)

import           Yesod



data MailFoundation = MailFoundation

mkYesod "MailFoundation" $(parseRoutesFile "mail-server-routes")

instance Yesod MailFoundation

-- | / HomeR GET
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Email
                                  Server|]
-- | /email EmailR POST
postEmailR :: Handler Value
postEmailR = do
   var <- parseJsonBody :: Handler (Result [EventEntries])
   case var of
     (Error f) -> return . toJSON $ f
     (Success s) -> do
        connection <- liftIO getConnection
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

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

decodeAR :: Text -> Maybe (AlarmRunner AnyAlarm AnyCall AnyCount)
decodeAR = decode.encodeUtf8.fromStrict

fetchAlarmRunnerStuff
  :: AlarmRunner AnyAlarm AnyCall ct -> AlarmEmailTemplate
fetchAlarmRunnerStuff (AlarmRunner { alarmTime = at'
                                   , alarmState = st
                                   , alarmParameters = ap, .. }) = AET at' (message ap) (alarm st) (getTo.getPeople.callList $ ap) (email . person $ st) (call st)
  where
    getTo = foldl fldFcn []
    fldFcn a b
     | emailAlert b = email b:a
     | otherwise =  a

buildEmailSubject :: AlarmEmailTemplate -> Text
buildEmailSubject aet =  aetName aet <>  (" "::Text) <>  (pack . statusHumanReadable $ aetStatus aet)

processMailList :: AlarmEmailTemplate -> Text -> IO Mail
processMailList aet email = do
  tz <- getCurrentTimeZone
  let fromAddress = defaultFromAddress
      toAddress = Address Nothing email
      emailSubject = buildEmailSubject aet
      plainBody = "here is the plainBody"
      htmlBody = hamletToText (alarmMailTemplate aet tz)
  sMail <- simpleMail fromAddress toAddress emailSubject  plainBody htmlBody  []
  return $ sMail {mailTo= [toAddress]}


processAlarmEmailTemplate :: AlarmEmailTemplate -> [Text] -> IO [Mail]
processAlarmEmailTemplate aet  = traverse (processMailList aet)


processAlarmRunner :: AlarmRunner AnyAlarm AnyCall ct -> SMTPConnection -> IO ()
processAlarmRunner ar connection = do
  let at' = fetchAlarmRunnerStuff ar
  let froms = mailPPL at'
  case froms of
      [] -> ePrint ("Email List is empty. Please check with system Administrator."::Text)
      _ -> do
           rms <- processAlarmEmailTemplate at' froms
           void $ authenticateMailClient connection
           void $ traverse (\rm -> sendEmails rm froms connection) rms
           return ()









