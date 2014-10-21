{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Plow.Email.ServerSpec
import           Test.Hspec            (hspec)
import           TestImport

main :: IO ()
main = hspec $
        yesodSpec MailFoundation plowemailspec
