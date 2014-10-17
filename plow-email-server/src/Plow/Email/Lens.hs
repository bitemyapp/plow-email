

{- |
Module      :  Plow.Email.Lens
Description :  Plow.Email.Lens 
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable 
Portability :   non-portable (System.Posix)


-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plow.Email.Lens ( stateChangeTime_
                       , stateChangeMsg_
                       , eventEntries_
                       , entryKey_
                       , _EventStateChange
                       ) where


import Control.Lens



import Plow.Extras.Lens (makeLenses_)
import Alarm.Log.Adapter



-- _eventEntries is defined in the Log Types so just use it

makeLenses_ ''StateChange
makeLenses_ ''EventEntries

makePrisms  ''LogEvent 
