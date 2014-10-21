{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport
    (  Spec
     , Example
     , module Yesod.Test
     , module Network.Mail.Mime
     , module Data.Aeson
     , MailFoundation (..)
     , mockOpts
     , exampleToAddress
     , exampleFromAddress
     , testSimpleMail
     , testAlarmRunner
     , testEventEntries
     ) where


-- General
import           Data.Aeson
import qualified Data.Set            as S
import           Data.Text           hiding (length)
import           Network.Mail.Mime
import           Plow.Email.CMD      (MyOptions (..))
import           Plow.Email.Handler  (MailFoundation (..))
import           Yesod.Test
-- Alarm Keys
import           Alarm.DB.Keys.Types
import           Alarm.Log.Adapter


type Spec = YesodSpec MailFoundation
type Example = YesodExample MailFoundation

mockOpts :: MyOptions
mockOpts = MyOptions Nothing

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


