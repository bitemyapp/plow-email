{-# LANGUAGE OverloadedStrings #-}

module Plow.Email.ServerSpec (main, spec) where

import Test.Hspec
import Network.HaskellNet.SMTP (Command (..) , sendMail, sendCommand)
import Network.HaskellNet.Auth (plain, AuthType (..))
import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SSL (defaultSettingsWithPort)



import Data.ByteString.Lazy (toStrict)
import Network.Mail.Mime

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False


exampleToAddress :: Address
exampleToAddress = Address (Just "Scott Murphy ") "scottmurphy09@gmail.com"

exampleFromAddress :: Address
exampleFromAddress = Address (Just "Alarms") "alarms@plowtech.net"

testSimpleMail :: IO Mail
testSimpleMail =  simpleMail exampleToAddress exampleFromAddress "cowboy" "here is the plain body" "<H1>Trial </H1>"  []

  
testConnectGmailSmtp = do  
  connection <- connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
  rm <- renderMail' =<< testSimpleMail
  sendCommand connection  (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
  sendMail  "alarms@plowtech.net" ["scottmurphy09@gmail.com"]  (toStrict rm)  connection
  
  
       
