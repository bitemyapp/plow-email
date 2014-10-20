{- |
Module      :  Plow.Email.Types
Description :  Types import from Alarm Log and Alarm State
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable
-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Plow.Email.Types (  module Alarm.Log.Adapter
                         , module AlarmState.Types
                         , AlarmEmailTemplate (..)
                         , defaultToAddress
                         , defaultFromAddress
                       ) where

import           Alarm.Log.Adapter
import           AlarmState.Types
import           Data.Text         (Text)
import           Network.Mail.Mime
import           Prelude

defaultToAddress :: Address
defaultToAddress = Address (Just "Scott Murphy ") "scottmurphy09@gmail.com"

defaultFromAddress :: Address
defaultFromAddress = Address (Just "Alarms") "alarms@plowtech.net"

data AlarmEmailTemplate = AET
                           { aetTime       :: Int,
                             aetName       :: Text,
                             aetStatus     :: AnyAlarm,
                             mailPPL       :: [Text],
                             aetCallPerson :: Text,
                             aetCallStatus :: AnyCall}
                           deriving (Show)
