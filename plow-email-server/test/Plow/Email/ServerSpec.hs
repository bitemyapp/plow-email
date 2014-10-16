{-# LANGUAGE OverloadedStrings #-}

module Plow.Email.ServerSpec (main, spec) where

import Test.Hspec
import Network.HaskellNet.SMTP (Command (..) , sendMail, sendCommand)
import Network.HaskellNet.Auth (plain, AuthType (..))
import Network.HaskellNet.SMTP.SSL 
import Network.HaskellNet.SSL (defaultSettingsWithPort)



main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False






-- exampleToAddress :: Address
-- exampleToAddress = Address (Just "Scott Murphy ") "scottmurphy09@gmail.com"

-- exampleFromAddress :: Address
-- exampleFromAddress = Address (Just "Alarms") "alarms@plowtech.net"

-- --testTextPart :: Part
-- testTextPart = plainTextPart "Howdy Partner"

-- --testSimpleMail :: Mail
-- testSimpleMail =  simpleMail exampleFromAddress [exampleToAddress] [] [] "Clear This out " [testTextPart] 


-- -- sendMailWithLogin' :: HostName -> PortNumber -> UserName -> Password -> Mail -> IO (
-- testSendMail = sendMailWithLogin' "smtp.gmail.com" 465 "alarms@plowtech.net" "wnpcq4edtr8k5mz" testSimpleMail 

  

testConnectGmailSmtp = do  
  connection <- connectSMTPSSLWithSettings "smtp.gmail.com" (defaultSettingsWithPort 465)
  sendCommand connection  (AUTH LOGIN "alarms@plowtech.net" "jk8kmyh4tv3cx4t")
  sendMail  "alarms@plowtech.net" ["scottmurphy09@gmail.com"]  "This is a test" connection
  
  
       
