{-# LANGUAGE MultiParamTypeClasses #-}
module Slim.SlimClientIO where
import Slim.Slim
import Slim.SlimClient
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString(hPutStrLn,hGet)
import System.IO(Handle,hClose,hSetBuffering,hGetLine,
                 hFlush,IOMode(..),BufferMode(..))
import Text.Printf
import System.Process
import System.Exit
import System.Info
import Control.Concurrent
import Control.Monad.State
import Data.Maybe
import Network.Socket
import Network.BSD

data IOSlimState = IOSlimState { slimHandle :: Maybe ProcessHandle
                               , config :: SlimConfig }

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
countAnswerChars h = hGet h 6 >>= return . readInt . toString

readAnswer :: Handle -> Int -> IO String
readAnswer h n = hGet h (n+1) >>= return . (( printf "%06d" n) ++) . toString 

sendToSlimAndClose :: Int -> [Instruction String] -> IO Answer
sendToSlimAndClose port insts = do cnx <- connectTo "localhost" (toInteger port)
                                   hSetBuffering cnx (BlockBuffering Nothing)
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