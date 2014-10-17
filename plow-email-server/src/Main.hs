{- |
Module      :  Main
Description :  Main
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable


-}

import           Plow.Email.Server

import           Yesod

main :: IO ()
main = startServer MailFoundation

startServer :: MailFoundation -> IO ()
startServer = warp 2633
