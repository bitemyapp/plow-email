{- |
Module      :  Plow.Email.MailTemplate
Description :  Email Template used to send to the client
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable
Portability :   non-portable
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Plow.Email.MailTemplate ( alarmMailTemplate
                               , statusHumanReable
                               , alarmMailTemplates
                               , hamletToText
                               , getCurrentTimeZone
                               ) where
import           Data.Time.LocalTime       (TimeZone, getCurrentTimeZone,
                                            utcToLocalTime)
import           Plow.Email.Types
import           Plow.Extras.Time          (intToUTCTime)
import           Prelude
import           Text.Hamlet               (Html, hamlet)
import           Text.Shakespeare.Template (hamletToText)

statusHumanReable :: AnyAlarm -> String
statusHumanReable status =
                  case status of
                  (SClear s) -> show s
                  (SClearing s) -> show s
                  (STripped s) -> show s
                  (STripping s) -> show s


alarmMailTemplates ::[AlarmEmailTemplate] -> t -> Html
alarmMailTemplates ars = [hamlet|
<h3> Plow Technologies Alarm System </h3>
<table>
  <tr>
    <th> Alarm Time
    <th> Alarm Name
    <th> Alarm State
  $forall   (AET at an as _ppl) <- ars
    <tr>
      <td> #{ at }
      <td> #{ an}
      <td> #{show as }

|]

alarmMailTemplate :: AlarmEmailTemplate -> TimeZone -> t-> Html
alarmMailTemplate  (AET at' an as _ppl) tz = do
  let utc' = intToUTCTime at'
      localTime = show $ utcToLocalTime tz utc'
      status = statusHumanReable as
  [hamlet|
<h3> Plow Technologies Alarm System </h3>
<table>
  <tr>
    <th> Alarm Time
    <th> Alarm Name
    <th> Alarm State
    <tr>
      <td> #{ localTime }
      <td> #{ an }
      <td> #{ status }

|]
