{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plow.Email.ServerSpec (main, spec) where


import           Test.Hspec

import           Control.Monad        (void)
import           Data.ByteString.Lazy (toStrict)
import           Network.Mail.Mime
import           TestImport


main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "Send Email" $
    it "Should Connect to the smtp gmail server and send a test email." $ do
      testConnectGmailSmtp
      testPostEmail



