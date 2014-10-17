{-# LANGUAGE OverloadedStrings #-}

module TestImport
    (  exampleToAddress
     , exampleFromAddress
     , testSimpleMail
     ) where


import           Network.HaskellNet.Auth     (AuthType (..), plain)
import           Network.HaskellNet.SMTP     (Command (..), sendCommand,
                                              sendMail)
import           Network.HaskellNet.SMTP.SSL
import           Network.HaskellNet.SSL      (defaultSettingsWithPort)

exampleToAddress :: Address
exampleToAddress = Address (Just "Scott Murphy ") "scottmurphy09@gmail.com"

exampleFromAddress :: Address
exampleFromAddress = Address (Just "Alarms") "alarms@plowtech.net"

testSimpleMail :: IO Mail
testSimpleMail =  simpleMail exampleToAddress exampleFromAddress "cowboy" "here is the plain body" "<H1>Trial </H1>"  []


-- mkTestFoundation = do
--   nmcs <- initializeSimpleStore "NodeManagerStates"
--   return NodeManager {nodes = nmcs}

-- readTestConf :: FilePath -> IO Value
-- readTestConf fPath = do
-- 	mfCont <- Y.decodeFile fPath :: IO (Maybe Value)
--         case mfCont of
--              Nothing -> fail "Error: Reading Config file."
--              Just fcont -> return fcont

-- testRetriveRequest :: Value
-- testRetriveRequest = object ["configName" .= ("alarm-state-config" :: String)]

-- testRetriveRequestWRewrite :: Value
-- testRetriveRequestWRewrite = object ["configName" .= ("alarm-state-config"::String) , "rewrite-rules" .= object ["key" .= ("host"::String) , "val".= (("http://why not working.com")::Value)]]

-- testCopyRequest :: Value
-- testCopyRequest = object ["route".=("http://127.0.0.1:3001/configure/add"::String)]

-- cloneList :: [String]
-- cloneList = ["alarm-state-config"]

-- testCloneRequest :: Value
-- testCloneRequest = object ["route".=("http://127.0.0.1:3001/configure/add"::String), "nameList" .= cloneList]

-- testDeleteRequest :: String
-- testDeleteRequest = "alarm-state-config"

-- testCloneDirRequest :: Value
-- testCloneDirRequest = object ["diretoryName" .= ("testConfigsDir"::String)]

-- type Spec = YesodSpec NodeManager
-- type Example = YesodExample NodeManager
