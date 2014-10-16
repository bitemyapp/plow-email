

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
module Plow.Email.Server () where




import           Yesod

data MailFoundation = MailFoundation

mkYesod "MailFoundation" [parseRoutes|
/ HomeR GET
/email EmailR POST
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Email
                                  Server|]


postEmailR :: Handler Value
postEmailR = do
   var <- parseJsonBody :: Handler Value
   return . toJSON $ var
  
main :: IO ()
main = warp 3000 HelloWorld

