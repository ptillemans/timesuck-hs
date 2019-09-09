{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Run (run) where

import Import

import Control.Lens ((.~), (<&>))

import Network.Google
import Network.Google.AppsCalendar
import Network.Google.Auth

import Data.Proxy     (Proxy (..))
import Data.Text      as T
import Data.Text.IO   as T
import System.Info    (os)
import System.Process (rawSystem)
import GHC.TypeLits   (Symbol)


redirectPrompt :: AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> IO (OAuthCode s)
redirectPrompt c p = do
  let url = formURL c p
  T.putStrLn $ "Opening URL " `T.append` url
  _ <- case os of
    "darwin" -> rawSystem "open"     [unpack url]
    "linux"  -> rawSystem "xdg-open" [unpack url]
    "mingw32" -> rawSystem "explorer" [unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine


clientId :: ClientId
clientId = ClientId "911970396904-hhbksdk352ee1isb7fp4tg9h6qdo8kn9.apps.googleusercontent.com"
clientSecret :: GSecret
clientSecret = GSecret "VwpMVKPBCZkLRTauGu55D-cZ"
oauthClient = OAuthClient clientId clientSecret


getCalendars :: IO CalendarList
getCalendars = do
  oauthCode <- redirectPrompt oauthClient calendarScope
  lgr <- newLogger Debug stdout
  mgr <- newManager tlsManagerSettings
  env <- newEnvWith 
          (installedApplication oauthClient oauthCode) 
          lgr mgr
          
  runResourceT . runGoogle env $ send calendarListList

run :: RIO App ()
run = do
  cals <- liftIO $ getCalendars
  logInfo "We're inside the application!"
  logInfo $ displayShow cals
