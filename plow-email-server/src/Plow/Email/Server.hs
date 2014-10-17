

{- |
Module      :  Plow.Email.Server
Description :  Plow.Email sends quick emails 
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable 
Portability :   non-portable (System.Posix)


-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards #-}
module Plow.Email.Server  where




import Alarm.Log.Adapter
import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import AlarmState.Types

import Control.Applicative ((<$>))
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (concat,Text,unpack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HaskellNet.Auth (plain, AuthType (..))
import Network.HaskellNet.SMTP (Command (..) , sendMail, sendCommand)
import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SSL (defaultSettingsWithPort)
import Network.Mail.Mime
import Plow.Email.Lens (stateChangeMsg_ , eventEntries_ , _EventStateChange)
import Prelude hiding (concat)
import Text.Hamlet
import Text.Shakespeare.Template
import Yesod.Core
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

isimpleMail :: [AlarmEmailTemplate] -> IO Mail
isimpleMail (a:aet) =  do
  sMail <- simpleMail exampleFromAddress (head tos)  "Alarm System Email"  "here is the plain body" (hamletToText $ alarmMailTemplate aet )   []
  return $ sMail {mailTo=tos}
   where
      tos = (Address Nothing) <$>
               mailPPL  a


decodeAR :: Text -> Maybe (AlarmRunner AnyAlarm AnyCall AnyCount)
decodeAR = decode.encodeUtf8.fromStrict
postEmailR :: Handler Value
postEmailR = do
   var <- parseJsonBody :: Handler (Result [EventEntries])
   case var of
     (Error f) -> return . toJSON $ f
     (Success s) -> do
       connection <- liftIO $ connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
       let eventList = s ^.. (traverse . eventEntries_ .folded ) :: [AlarmLogEvent]
           msgTxt = eventList ^.. (traverse  . _EventStateChange . stateChangeMsg_ )  
           ar = catMaybes $ (decodeAR) <$> msgTxt
           aets = fmap fetchAlarmRunnerStuff $ ar
           froms = L.concat $ (fmap unpack).mailPPL <$> aets
       rm <- liftIO $ renderMail' =<< (isimpleMail $ aets)
       liftIO $ sendCommand connection  (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
       liftIO $ sendMail  "alarms@plowtech.net" froms  (toStrict rm) connection              
       return . toJSON $ s

testThing :: Text
testThing = "test"

data AlarmEmailTemplate = AET
                           { aetTime :: Int,
                             aetName :: Text,
                             aetStatus :: AnyAlarm,
                             mailPPL :: [Text] }

fetchAlarmRunnerStuff
  :: AlarmRunner AnyAlarm c ct -> AlarmEmailTemplate
fetchAlarmRunnerStuff (AlarmRunner { alarmTime = at 
                                   , alarmState = st 
                                   , alarmParameters = ap, .. }) = AET at (message ap) (alarm st) (getTo.getPeople.callList $  ap)
  where
    getTo ars = foldl fldFcn [] ars
    fldFcn a b
     | emailAlert b = (email b):a
     | otherwise =  a


                          
alarmMailTemplate ::[AlarmEmailTemplate] -> t -> Html
alarmMailTemplate  ars = [hamlet|
<h1> Plow Technologies Alarm System </h1> 
<table>
  <tr>
    <th> Alarm Time
    <th> Alarm Name
    <th> Alarm State
  $forall   (AET at an as ppl) <- ars
    <tr>
      <td> #{ at }
      <td> #{ an}
      <td> #{show as }
    
|]
