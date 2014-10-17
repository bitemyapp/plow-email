

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




import           Alarm.Log.Adapter
import           AlarmState.Types
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy        (toStrict)

import           Control.Applicative         ((<$>))
import           Control.Exception           (SomeException, try)
import           Control.Monad               (void)
import qualified Data.List                   as L
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text, concat, unpack)
import           Data.Text.Lazy              (fromStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Data.Time.LocalTime
import           Network.HaskellNet.Auth     (AuthType (..), plain)
import           Network.HaskellNet.SMTP     (Command (..), SMTPConnection,
                                              sendCommand, sendMail)
import           Network.HaskellNet.SMTP.SSL
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)
import           Network.Mail.Mime
import           Plow.Email.Lens             (eventEntries_, stateChangeMsg_,
                                              _EventStateChange)
import           Plow.Extras.Time
import           Prelude                     hiding (concat)
import           System.IO                   (hPrint, stderr)
import           Text.Hamlet
import           Text.Shakespeare.Template
import           Yesod
-- Test Stuff
import           Alarm.DB.Keys
import           Alarm.DB.Keys.Types
import qualified Data.Set                    as S
import           DirectedKeys.Types

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


exampleToAddress :: Address
exampleToAddress = Address (Just "Scott Murphy ") "scottmurphy09@gmail.com"

exampleFromAddress :: Address
exampleFromAddress = Address (Just "Alarms") "alarms@plowtech.net"


processMailList :: AlarmEmailTemplate -> Text -> IO Mail
processMailList aet email = do
  tz <- getCurrentTimeZone
  sMail <- simpleMail exampleFromAddress to' "Alarm System Email"  "here is the plain body" (hamletToText $ alarmMailTemplate aet tz )  []
  return $ sMail {mailTo=to':[]}
    where
      to' = Address Nothing email

processAlarmEmailTemplate :: AlarmEmailTemplate -> [Text] -> IO [Mail]
processAlarmEmailTemplate aet froms = traverse (\email -> processMailList aet email) froms

decodeAR :: Text -> Maybe (AlarmRunner AnyAlarm AnyCall AnyCount)
decodeAR = decode.encodeUtf8.fromStrict

processAlarmRunner :: AlarmRunner AnyAlarm c ct -> SMTPConnection -> IO ()
processAlarmRunner ar connection = do
  let at = fetchAlarmRunnerStuff ar
  let froms = mailPPL at
  case froms of
      [] -> ePrint ("Email List is empty. Please check with system Administrator."::Text)
      _ -> do
           rms <-  processAlarmEmailTemplate at (froms)
           void $ liftIO $ sendCommand connection  (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
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
            ars = catMaybes $ (decodeAR) <$> msgTxt
            alarmRunnerCount = length ars
        rslt <- liftIO $ try $ traverse (`processAlarmRunner` connection) ars
        case rslt of
            Left (_e::SomeException) -> return . toJSON $ ("Error on sending email request."::Text)
            Right _ -> do
                  liftIO $ print ((show alarmRunnerCount ++ " AlarmRunners are process.")::String)
                  return . toJSON $ s

testKey1 :: DirectedKeyRaw AlarmKeyAId AlarmKeySrc AlarmKeyDest AlarmKeyTime
testKey1 = DKeyRaw (AlarmKeyAId "Test Key 1" ) (AlarmKeySrc . SText $ "onping.aacs-us.com") (AlarmKeyDest . SText $ "onping.aacs-us.com") (AlarmKeyTime 1399385432)

testEventEntry :: EventEntries
testEventEntry = EventEntries {
       entryKey = testKey1
     , eventEntries = testEntries
     }


testAlarmRunner :: Text
testAlarmRunner = ("{\"state\":{\"call\":{\"tag\":\"SCalling\",\"contents\":[]},\"person\":{\"email\":\"malvarez@atlasenergy.com\",\"textAlert\":false,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":5804839558,\"emailAlert\":false},\"count\":{\"tag\":\"SCountMore\",\"contents\":{\"getMore\":0}},\"alarm\":{\"tag\":\"SClear\",\"contents\":[]}},\"time\":1412892884,\"active\":true,\"parameters\":{\"clearTime\":2,\"tripTime\":5,\"recallTime\":60,\"callList\":{\"getPeople\":[{\"email\":\"lingpo.huang@plowtech.net\",\"textAlert\":false,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":5804839558,\"emailAlert\":false},{\"email\":\"lingpohuang7@gmail.com\",\"textAlert\":true,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":4058317443,\"emailAlert\":false}]},\"message\":\"Collie 2-18H Swf Communication Loss\"},\"val\":\"0\",\"alarmId\":\"AAAAAAAAAGUfiwgAAAAAAAADY2AAA2mlfFNjczMzI8sU0ySLNAMzoyTzZAMgSExTgqhgEM7PK8jMS9cryMkvL0lNztDLSy2BSokYGljoGZpZ6BmZGOgZGhlbGRkZG4MkguN5YwCd8THJYgAAAA==\",\"timeL\":0}"::Text)

testEntries :: S.Set (LogEvent StateChange b c)
testEntries = S.singleton $ EventStateChange (StateChange (1274335854::Int) testAlarmRunner)

testEventEntries :: [EventEntries]
testEventEntries = [testEventEntry]

testPostEmail :: IO ()
testPostEmail = do
   let s = testEventEntries
   print (toJSON s)
   connection <- liftIO $ connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
   let eventList = s ^.. (traverse . eventEntries_ .folded ) :: [AlarmLogEvent]
       msgTxt = eventList ^.. (traverse  . _EventStateChange . stateChangeMsg_ )
       ars = catMaybes $ decodeAR <$> msgTxt
       alarmRunnerCount = length ars
   rslt <- liftIO $ try $ traverse (`processAlarmRunner` connection) ars
   case rslt of
    Left (_e::SomeException) -> print  ("Error on sending email request."::Text)
    Right _ -> do
      liftIO $ print ((show alarmRunnerCount ++ " AlarmRunners are process.")::String)
      print ("Email Sucessfully sent."::Text)

testThing :: Text
testThing = "test"

data AlarmEmailTemplate = AET
                           { aetTime   :: Int,
                             aetName   :: Text,
                             aetStatus :: AnyAlarm,
                             mailPPL   :: [Text] }
                           deriving (Show)

fetchAlarmRunnerStuff
  :: AlarmRunner AnyAlarm c ct -> AlarmEmailTemplate
fetchAlarmRunnerStuff (AlarmRunner { alarmTime = at'
                                   , alarmState = st
                                   , alarmParameters = ap, .. }) = AET at' (message ap) (alarm st) (getTo.getPeople.callList $  ap)
  where
    getTo ars = foldl fldFcn [] ars
    fldFcn a b
     | emailAlert b = email b:a
     | otherwise =  a


alarmMailTemplates ::[AlarmEmailTemplate] -> t -> Html
alarmMailTemplates  ars = [hamlet|
<h1> Plow Technologies Alarm System </h1>
<table>
  <tr>
    <th> Alarm Time
    <th> Alarm Name
    <th> Alarm State
  $forall   (AET at an as _ppl) <- ars
    <tr>
      <td> #{ at }
      <td> #{ an}
      <td> #{show as }

|]

alarmMailTemplate :: AlarmEmailTemplate -> TimeZone -> t-> Html
alarmMailTemplate  (AET at' an as _ppl) tz = do
  let utc' = intToUTCTime $ at'
      localTime = show $ utcToLocalTime tz utc'
  [hamlet|
<h1> Plow Technologies Alarm System </h1>
<table>
  <tr>
    <th> Alarm Time
    <th> Alarm Name
    <th> Alarm State
    <tr>
      <td> #{ localTime }
      <td> #{ an }
      <td> #{show as }

|]
