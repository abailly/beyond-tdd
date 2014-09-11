{-# LANGUAGE FlexibleInstances #-}

module Slim.SlimServer where

import Slim.Slim
import Slim.SlimClientIO
import Control.Monad.State hiding (void)
import qualified Data.Map as M
import Data.Map((!))
import Data.ByteString.UTF8 (fromString)
import Data.ByteString(hPutStrLn,hGet)
import System.IO(hSetBuffering,
                 hFlush,IOMode(..),BufferMode(..))
import Control.Applicative
import Network.Socket
import Control.Concurrent(forkIO,ThreadId)
import Control.Arrow

-- |Starts a slim server on given port and returns this port
startSlimServer :: Int -> IO ThreadId
startSlimServer port = 
  (forkIO $ doServe port)
  
doServe :: Int -> IO ()  
doServe port =
  do address <- inet_addr "127.0.0.1"
     sock <- socket AF_INET Stream defaultProtocol
     let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
     bindSocket sock addr
     listen sock 5
     loopOn sock
     
loopOn :: Socket -> IO ()
loopOn sock = do (client, clientAddress) <- accept sock
                 answerRequest client
                 sClose client
                 return ()  -- should be a loop

answerRequest :: Socket -> IO ()
answerRequest sock = do hdl <- socketToHandle sock ReadWriteMode 
                        hSetBuffering hdl (BlockBuffering Nothing)
                        calls <- (countAnswerChars hdl >>= readAnswer hdl)
                        let Just calls' = (decode calls) :: Maybe [Instruction String]
                        let st = M.empty :: M.Map String Div
                        let answer =  invoke calls' st
                        hPutStrLn hdl ((fromString . encode) answer)
                        hFlush hdl
                        return ()

class Invokable i where
  call :: Instruction String -> State i Answer
  make :: [ String ] -> i
    
instance (Invokable i) => Invokable (M.Map String i) where
  call c@(Call x instanc t a) = do m <- get
                                   let (result, st) = tryCall c m
                                   case st of 
                                     Nothing -> put m
                                     Just s' -> put $ M.insert instanc s' m
                                   return $ result
  call (Make x  n _ p) = do m <- get
                            put $ M.insert n (make p) m
                            return $ ok x
  make _ = M.empty

tryCall c@(Call x instanc t a) state = case M.lookup instanc state  of 
  Nothing  -> (exception x ***  id) ( "NO_INSTANCE " ++ instanc, Nothing)
  Just obj -> (id *** Just) $ runState (call c) obj
  
invoke :: (Invokable s) => [ Instruction String ] -> s -> Answer
invoke calls = answers . evalState (sequence $ map call calls) 

answers :: [ Answer ] -> Answer
answers as = A $ L (answers' as)
  where
    answers' []         = []
    answers' ((A a):as) = a : answers' as 
    
void x = A (L [ S x, S "/__VOID__/"])
exception x m = A (L [ S x, S ("__EXCEPTION__: " ++ m)])
ok   x = A ( L [S x , S "OK"] )

val :: (Show v) => String -> v -> Answer
val  x v = A ( L [S x , S $ show v] )
  
-- |Division data-type, used for testing purpose much like the classical eg.Division 
-- from standard fitnesse documentation.
-- It should be possible to generate a lot of stuff using Template Haskell...

data Div = Div { numerator :: Double, 
                 denominator :: Double }                           
         deriving (Eq,Show)

setNumerator   d div = div { numerator   = d } 
setDenominator d div = div { denominator = d } 
getNumerator         = numerator 
getDenominator       = denominator
quotient  d          = numerator d / denominator d

readDouble :: String -> Double
readDouble s = x where (x,_):_ = (reads :: String -> [(Double,String)]) s

setter x f s = get >>= (put . f (readDouble s)) >> (return $ void x)
getter x f = get >>= return . val x . f

instance Invokable Div where
  call (Call x n "setNumerator" [ s ])   = setter x setNumerator s
  call (Call x n "setDenominator" [ s ]) = setter x setDenominator s
  call (Call x n "getNumerator" [])   = getter x getNumerator
  call (Call x n "getDenominator" []) = getter x getDenominator
  call (Call x  n "quotient" [])         = get >>= return . val x . quotient
  call (Call x  n m _)                   = return $ exception x ("NO_METHOD_IN_CLASS " ++ m ++ " " ++ n)
  call (Make x  n "eg.Division" [])      = do put $ Div 0 0
                                              return $ ok x
  make _                                 = Div 0 0
