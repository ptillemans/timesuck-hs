{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

import Control.Lens ((.~), (<&>))

import Network.Google
import Network.Google.AppsCalendar



getCalendars :: IO CalendarList
getCalendars = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ calendarScope)
  runResourceT . runGoogle env $ send calendarListList

run :: RIO App ()
run = do
  cals <- liftIO $ getCalendars
  logInfo "We're inside the application!"
  logInfo $ displayShow cals
