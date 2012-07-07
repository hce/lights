{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.IO
import Network
import Data.List
import Network.Wai.EventSource
import Control.Concurrent
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)

import Foundation
import ControlState

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler RepHtml
getHomeR = do
  cc <- fmap controlState getYesod  
  portStatus <- liftIO $ getPortStatus cc
  case portStatus of
    ControlStatus crtlst -> withPortsOn crtlst
    ControlOff -> withPortsOff
    
    where 
      
      withPortsOn ports = do
        let Just lightsOn = ports !! 0
        defaultLayout $ do
          aDomId <- lift newIdent
          setTitle "lindorf turns the lites onoroff"
          $(widgetFile "homepage")
          
      withPortsOff = defaultLayout $ do
          setTitle "Lindorf is turned off"
          [whamlet|
           <p>The NetAVR device is turned off.
           <p>Please turn it on or lindorf.de will not be able to work.
                  |]
                          
getFeedR :: Handler RepHtml
getFeedR = do
  cs <- fmap controlState getYesod
  ec <- liftIO $ getEventChan cs
  -- Can't really do this because it'd be duplicated to every instance of the channel
  -- liftIO $ writeChan ec $ ServerEvent (Just $ fromText "welcome") Nothing [fromText "Good to have you"]
  req <- waiRequest
  res <- lift $ eventSourceAppChan ec req
  sendWaiResponse res
  
            

postControlR :: Handler RepHtml
postControlR = do
  (turnOn:turnOff:[]) <- mapM lookupPostParam ["lightOn", "lightOff"]
  cc <- fmap controlState getYesod
  case (turnOn, turnOff) of
    (Just _, Nothing) -> do
      liftIO $ queueCommand cc (SwitchOn 1)
      redirect HomeR
    (Nothing, Just _) -> do
      liftIO $ queueCommand cc (SwitchOff 1)
      redirect HomeR
    _ ->
      defaultLayout [whamlet|
                     <p>Invalid input #{show turnOn}
                            |]
      
    