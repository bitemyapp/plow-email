{- |
Module      :  Plow.Email.Types
Description :  Types import from Alarm Log and Alarm State
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Plow.Email.Types (  module Alarm.Log.Adapter
                         , module AlarmState.Types
                         , AlarmEmailTemplate (..)
                       ) where

import           Alarm.Log.Adapter
import           AlarmState.Types
import           Data.Text         (Text)


data AlarmEmailTemplate = AET
                           { aetTime       :: Int,
                             aetName       :: Text,
                             aetStatus     :: AnyAlarm,
                             mailPPL       :: [Text],
                             aetCallPerson :: Text,
                             aetCallStatus :: AnyCall}
                           deriving (Show)
