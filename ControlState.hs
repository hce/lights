{-# LANGUAGE OverloadedStrings #-}

module ControlState where

import Prelude
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Monoid (mconcat)
import Network
import System.IO
import System.IO.Error
import System.Timeout
import Network.Socket
import Network.Wai.EventSource
import Data.Text (Text)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)


data ControlStatus  = ControlOff | ControlStatus [Maybe Bool]
                    deriving (Eq, Show)

data ControlCommand = SwitchOn Int | SwitchOff Int
                    deriving (Eq, Show)
                             
type ConnectCmd = IO Handle

data ControlState = ControlState {
  threadID       :: Maybe ThreadId,
  statusVar      :: TVar ControlStatus,
  cmdChan        :: Chan ControlCommand,
  handle         :: Maybe Handle,
  connectCommand :: ConnectCmd,
  eventChan      :: Chan ServerEvent
  }
                    
beginIt :: String -> IO ControlState
beginIt host = do
  sv <- atomically $ newTVar ControlOff
  cc <- newChan
  h <- reconnect connectCmd
  sec <- newChan
  tid <- forkIO $ checkIt (ControlState Nothing sv cc h connectCmd sec)
  return $ ControlState (Just tid) sv cc h connectCmd sec
  
  where
    connectCmd = connectTo host $ PortNumber 50290
    
getEventChan :: ControlState -> IO (Chan ServerEvent)
getEventChan = dupChan . eventChan

portStatesToText :: ControlStatus -> Builder
portStatesToText ControlOff = fromText "Lindorf Control is off"
portStatesToText (ControlStatus stati) = mconcat $ fmap (fromText . stu) stati
  where
    stu :: Maybe Bool -> Text
    stu Nothing = "-"
    stu (Just True) = "1"
    stu (Just False) = "0"

fireEvent :: ControlState -> Maybe Builder -> Maybe Builder -> [Builder] -> IO ()
fireEvent cs eventName eventID eventData = do
  writeChan chan $ ServerEvent eventName eventID eventData
  _ <- readChan chan
  return ()
  where
    chan = eventChan cs
  
reconnect :: ConnectCmd -> IO (Maybe Handle)
reconnect cc = 10000000 `timeout` doConnect >>= return . join
  where 
    doConnect = (fmap Just cc) `catch` \_ -> return Nothing

checkIt :: ControlState -> IO ()
checkIt cs = do
  maybeCommand <- timeout 5000000 $ readChan $ cmdChan cs
  case (maybeCommand, handle cs) of 
    (Just command, Just h) -> do
      fireEvent cs Nothing Nothing [fromString $ show command]
      performCommand h command `catch` oer
      checkCR `catch` oer
    (_, _) -> return ()
  
  ps <- gps `catch` \_ -> return ControlOff
  updatedPS <- atomically $ do
    oldPS <- readTVar (statusVar cs)
    if oldPS /= ps
      then writeTVar (statusVar cs) ps >> return True
      else return False
           
  when updatedPS $ fireEvent cs Nothing Nothing [portStatesToText ps]
  
  cs2 <- if ps /= ControlOff  then return cs else do
    putStrLn "Trying to reconnect!"
    maybeNewSock <- reconnect (connectCommand cs)
    return cs {handle = maybeNewSock}
  
  checkIt cs2
  
  where
    oer :: IOError -> IO ()
    oer _ = return ()
    
    checkCR = do
      Just h <- return $ handle cs
      response <- hGetLine h
      putStr "Got a command, result is: "
      putStrLn response
      
    gps = do
      Just h <- return $ handle cs
      getPortStati h >>= return . ControlStatus
  
getPortStatus :: ControlState -> IO ControlStatus
getPortStatus = atomically . readTVar . statusVar
  
queueCommand :: ControlState -> ControlCommand -> IO ()
queueCommand cs = writeChan (cmdChan cs)
    
performCommand :: Handle -> ControlCommand -> IO ()
performCommand h (SwitchOn n)  = (hPutStrLn h $ "setport " ++ (show n) ++ ".1") >> hFlush h
performCommand h (SwitchOff n) = (hPutStrLn h $ "setport " ++ (show n) ++ ".0") >> hFlush h

turnLightsOn :: Handle -> Int -> IO ()
turnLightsOn h n =  hPutStrLn h $ "setport " ++ (show n) ++ ".1"
  
turnLightsOff :: Handle -> Int -> IO ()
turnLightsOff h n =  hPutStrLn h $ "setport " ++ (show n) ++ ".0"
  
  
--getPortStatus :: Int -> IO (Maybe Bool)
--getPortStatus n =  
--  getPortStati >>= return . last . take n

getPortStati :: Handle -> IO [Maybe Bool]
getPortStati h = do
  hPutStrLn h "getstatus"
  hFlush h
  ('S':statusRaw) <- hGetLine h
  return . (map charToStatus) . tail . reverse $ statusRaw
    where
      charToStatus c =
        case c of
          '0' -> Just False
          '1' -> Just True
          _ -> Nothing