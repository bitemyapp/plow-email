{- |
Module      :  <ServerSpec>
Description :  <Warp Specs for Plow Email Server>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable (System.Posix)
-}

{-# LANGUAGE OverloadedStrings #-}
module Plow.Email.ServerSpec (plowemailspec) where


import           Plow.Email.Handler
import           TestImport

plowemailspec :: Spec
plowemailspec = do
  ydescribe "getHomeR" $
      yit "See if Plow Email Server is running right" $ do
          get HomeR
          printBody >> statusIs 200
  ydescribe "postEmailR" $
      yit "Send a email using defualt setting " $ do
          postBody EmailR (encode testEventEntries)
          printBody >> statusIs 200
