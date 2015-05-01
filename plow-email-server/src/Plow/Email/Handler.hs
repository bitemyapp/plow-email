
{- |
Module      :  Plow.Email.Handler
Description :  Routes and its implementation
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable (System.Posix)
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Plow.Email.Handler  where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Exception       (SomeException, try)
import           Control.Lens            (folded, traverse, (^..))
import           Control.Monad           (void)
import           Data.Aeson
import           Data.Maybe              (catMaybes)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           Data.Text.Lazy          (fromStrict)
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Mail.Mime       (Mail (..))
import           Plow.Email.Lens         (eventEntries_, stateChangeMsg_,
                                          _EventStateChange)
import           Plow.Email.MailClient
import           Plow.Email.MailTemplate
import           Plow.Email.Types
import           System.IO               (hPrint, stderr)
import           Text.Hamlet             (Html)
import           Yesod.Core              (Yesod, defaultLayout, liftIO,
                                          parseJsonBody, renderRoute, whamlet)
import           Yesod.Core.Dispatch     (mkYesod, parseRoutesFile)


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
     (Success ees) -> do
        connection <- liftIO getConnection
        processAlarmRunners (eventEntriesToAlarmRunners ees) connection ees


postReportEmailR :: Handler Value
postReportEmailR = do
  var <- parseJsonBody :: Handler (Result SimpleMail)
  case var of
     (Error f) -> return . toJSON $ f
     (Success mail) -> do
        connection <- liftIO getConnection
        liftIO $ mySimpleMail mail connection
        return . toJSON $ ()

mySimpleMail :: SimpleMail -> SMTPConnection -> IO ()
mySimpleMail sm conn = do
        sMail' <- simpleMail
                   (fromAddress sm)
                   ( toAddress sm )
                   ( subject sm )
                   ( plainText sm)
                   ( htmlText sm)
                   ( attachments sm)
        let sMail = sMail' {mailCc = [], mailBcc = [], mailTo=[toAddress sm]}
        void $ authenticateMailClient conn
        processMailAnySender sMail fromAddr toAddr conn
         where
           fromAddr = addressEmail . fromAddress $ sm
           toAddr = addressEmail . toAddress $ sm
data SimpleMail = SimpleMail {
               fromAddress  :: Address,
                toAddress   :: Address,
                subject     :: Text,
                plainText   :: TL.Text,
                htmlText    :: TL.Text,
                attachments :: [(Text,FilePath)]
} deriving (Show)

instance FromJSON SimpleMail where
  parseJSON (Object o) = SimpleMail <$>
                          ((Address Nothing) <$> (o .: "from" ) ) <*>
                          ((Address Nothing) <$> (o .: "to" ) ) <*>
                          o .: "subject" <*>
                          o .: "plainText" <*>
                          o .: "htmlText" <*>
                          o .: "attachments"

  parseJSON _ = fail "Rule expected object received other"

instance ToJSON SimpleMail where
   toJSON (SimpleMail (Address _ from)
                      (Address _ to)
                      subject
                      plainText
                      htmlText
                      attachments ) = object [ "from" .= from
                                               ,"to".= to
                                               ,"subject" .= subject
                                               ,"plainText" .= plainText
                                               ,"htmlText" .= htmlText
                                               ,"attachments" .= attachments]

-- ==============Handler Function===========

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
  return sMail {mailCc = [], mailBcc = [], mailTo=[toAddress]}

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
           void $ traverse (\rm -> sendEmails rm connection) rms
           return ()


eventEntriesToAlarmRunners :: [EventEntries] -> [AlarmRunner AnyAlarm AnyCall AnyCount]
eventEntriesToAlarmRunners s = catMaybes $ decodeAR <$> msgTxt
     where  eventList = s ^.. (traverse . eventEntries_ .folded ) :: [AlarmLogEvent]
            msgTxt = eventList ^.. (traverse  . _EventStateChange . stateChangeMsg_ )



processAlarmRunners :: [AlarmRunner AnyAlarm AnyCall ct] -> SMTPConnection -> [EventEntries] -> Handler Value
processAlarmRunners ars connection s = do
  let alarmRunnerCount = length ars
  rslt <- liftIO $ try $ traverse (`processAlarmRunner` connection) ars
  case rslt of
       Left (_e::SomeException) -> return . toJSON $ ("Error on sending email request."::Text)
       Right _ -> do
         liftIO $ print ((show alarmRunnerCount ++ " AlarmRunners are process.")::String)
         return . toJSON $ s




