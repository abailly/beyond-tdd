{-# LANGUAGE MultiParamTypeClasses #-}
module Slim.SlimClientIO where

import           Control.Concurrent
import           Control.Monad.State
import           Data.ByteString      (hGet, hPutStrLn)
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Maybe
import           Network.BSD
import           Network.Socket
import           Slim.Slim
import           Slim.SlimClient
import           System.Exit
import           System.Info
import           System.IO            (BufferMode (..), Handle, IOMode (..),
                                       hClose, hFlush, hGetLine, hSetBuffering)
import qualified System.IO            as IO
import           System.Process
import           Text.Printf

data IOSlimState = IOSlimState { slimHandle :: Maybe ProcessHandle
                               , config     :: SlimConfig }

instance SlimState IOSlimState where
  slimConfig = config

-- | Connect to the given host/port
connectTo :: String -> Integer -> IO Handle
connectTo host port = do address <- inet_addr "127.0.0.1"
                         sock <- socket AF_INET Stream defaultProtocol
                         setSocketOption sock KeepAlive 1
                         connect sock (SockAddrInet ((fromIntegral port) :: PortNumber) address)
                         socketToHandle sock ReadWriteMode

countAnswerChars :: Handle -> IO Int
countAnswerChars h = do
  c <- hGet h 6
  putStrLn $ "counting answer chars " ++ toString c
  return $ readInt (toString c)

countAnswerCharsLog :: Handle ->  Handle -> IO Int
countAnswerCharsLog log h = do
  c <- hGet h 6
  IO.hPutStrLn log $ "counting answer chars " ++ (toString c)
  return $ readInt (toString c)

readAnswer :: Handle -> Int -> IO String
readAnswer h n = hGet h (n+1) >>= return . (( printf "%06d" n) ++) . toString

sendToSlimAndClose :: Int -> [Instruction String] -> IO Answer
sendToSlimAndClose port insts = do cnx <- connectTo "localhost" (toInteger port)
                                   hSetBuffering cnx (BlockBuffering Nothing)
                                   hGetLine cnx
                                   hPutStrLn cnx ((fromString . encode) insts)
                                   hFlush cnx
                                   returns <- (countAnswerChars cnx >>= readAnswer cnx)
                                   hClose cnx
                                   let Just answer = decode  returns
                                   return answer


instance SlimIO IO IOSlimState where
    ioTerminate st = do liftIO $ putStrLn "Waiting for termination of Slim"
                        liftIO $ waitForProcess . fromJust . slimHandle $ st

    fetchAnswers st msgs = liftIO $! do let port = slimport $ slimConfig st
                                        -- connect using low-level Network.Socket
                                        cnx <- connectTo "localhost" port
                                        hSetBuffering cnx (BlockBuffering Nothing)
                                        hGetLine cnx
                                        let calls = (fromString msgs)
                                        hPutStrLn cnx calls
                                        if (verbose $ slimConfig st) then putStrLn $ toString calls else return ()
                                        hFlush cnx
                                        returns <- (countAnswerChars cnx >>= readAnswer cnx)
                                        if (verbose $ slimConfig st) then putStrLn returns else return ()
                                        return returns


    doStartSlim state = do pr <- liftIO $ runProcess  exe ["-cp", classpath, "fitnesse.slim.SlimService", port] wd Nothing Nothing Nothing Nothing
                           liftIO $ threadDelay 500000
                           st <- get
                           put st { slimHandle = Just pr }
        where
          config        = (slimConfig state)
          exe           = slimexecutable config
          wd            = Just $ executiondir config
          port          = show (slimport config)
          pathSeparator = case os of
                            "windows" -> ";"
                            "mingw32" -> ";"
                            _         -> ":"
          classpath     = foldl (\ x y -> x ++ pathSeparator ++ y) "." (slimclasspath config)


defaultSlim   = IOSlimState  Nothing defaultConfig
