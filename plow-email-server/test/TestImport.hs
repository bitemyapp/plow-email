{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport
    (  module Network.Mail.Mime
     , exampleToAddress
     , exampleFromAddress
     , testSimpleMail
     , testAlarmRunner
     , testEventEntries
     , testConnectGmailSmtp
     , testPostEmail
     ) where


import           Data.Text                   hiding (length)
import           Network.Mail.Mime
-- Alarm Keys
import           Alarm.DB.Keys
import           Alarm.DB.Keys.Types
import           Alarm.Log.Adapter
import qualified Data.Set                    as S
import           DirectedKeys.Types
-- Key
import           Plow.Email.Server           hiding (exampleFromAddress,
                                              exampleToAddress)
import           Prelude                     hiding (concat)
-- Email
import           Control.Applicative         ((<$>))
import           Control.Exception           (SomeException, try)
import           Control.Lens
import           Control.Monad               (void)
import           Data.Aeson
import           Data.ByteString.Lazy        (toStrict)
import           Data.Maybe                  (catMaybes)
import           Data.Traversable            (traverse)
import           Network.HaskellNet.Auth     (AuthType (..), plain)
import           Network.HaskellNet.SMTP     (Command (..), sendCommand,
                                              sendMail)
import           Network.HaskellNet.SMTP.SSL
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)
import           Plow.Email.Lens             (eventEntries_, stateChangeMsg_,
                                              _EventStateChange)
import           Yesod


exampleToAddress :: Address
exampleToAddress = Address (Just "Scott Murphy ") "lingpo.huang@plowtech.net"

exampleFromAddress :: Address
exampleFromAddress = Address (Just "Alarms") "alarms@plowtech.net"

testSimpleMail :: IO Mail
testSimpleMail =  simpleMail exampleToAddress exampleFromAddress "Email Test" "testConnectGamilsSmtp Passed." "<H1>Trial </H1>"  []

testAlarmRunner :: Text
testAlarmRunner = "{\"state\":{\"call\":{\"tag\":\"SCalling\",\"contents\":[]},\"person\":{\"email\":\"malvarez@atlasenergy.com\",\"textAlert\":false,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":5804839558,\"emailAlert\":true},\"count\":{\"tag\":\"SCountMore\",\"contents\":{\"getMore\":0}},\"alarm\":{\"tag\":\"SClear\",\"contents\":[]}},\"time\":1412892884,\"active\":true,\"parameters\":{\"clearTime\":2,\"tripTime\":5,\"recallTime\":60,\"callList\":{\"getPeople\":[{\"email\":\"lingpo.huang@plowtech.net\",\"textAlert\":false,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":5804839558,\"emailAlert\":true},{\"email\":\"lingpohuang7@gmail.com\",\"textAlert\":true,\"callCount\":1,\"callAlert\":true,\"phoneNumber\":4058317443,\"emailAlert\":false}]},\"message\":\"Collie 2-18H Swf Communication Loss\"},\"val\":\"0\",\"alarmId\":\"AAAAAAAAAGUfiwgAAAAAAAADY2AAA2mlfFNjczMzI8sU0ySLNAMzoyTzZAMgSExTgqhgEM7PK8jMS9cryMkvL0lNztDLSy2BSokYGljoGZpZ6BmZGOgZGhlbGRkZG4MkguN5YwCd8THJYgAAAA==\",\"timeL\":0}"

testKey1 :: DirectedKeyRaw AlarmKeyAId AlarmKeySrc AlarmKeyDest AlarmKeyTime
testKey1 = DKeyRaw (AlarmKeyAId "Test Key 1" ) (AlarmKeySrc . SText $ "onping.aacs-us.com") (AlarmKeyDest . SText $ "onping.aacs-us.com") (AlarmKeyTime 1399385432)

testEventEntry :: EventEntries
testEventEntry = EventEntries {
       entryKey = testKey1
     , eventEntries = testEntries
     }

testEntries :: S.Set (LogEvent StateChange b c)
testEntries = S.singleton $ EventStateChange (StateChange (1274335854::Int) testAlarmRunner)

testEventEntries :: [EventEntries]
testEventEntries = [testEventEntry]

testConnectGmailSmtp :: IO ()
testConnectGmailSmtp = do
  connection <- connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
  rm <- renderMail' =<< testSimpleMail
  void $ sendCommand connection  (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
  sendMail  "alarms@plowtech.net" ["lingpo.huang@plowtech.net"]  (toStrict rm)  connection


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