{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Slim.SlimClient(slimClient
                 ,doSlim
                 ,Slim(..)
                 ,SlimConfig(..)
                 ,SlimState(..)
                 ,defaultConfig
                 ,runSlim
                 ,startSlim
                 ,doSendSlim
                 ,endSlim
                 ,SlimIO(..)
)
where
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Slim.Slim
import           System.Exit         (ExitCode)

-- |Abstract state of the Slim backend.
-- Provides configuration information.
class SlimState sst where
  slimConfig :: sst -> SlimConfig

-- | A class for abstracting low-level I/O of slim with a given backend
-- Its main purpose is to make testing of Slim Input/Output easier by
-- adding another level of abstraction with a default instantiation to
-- standard IO monad.
class (MonadIO m, SlimState st) => SlimIO m st where
    -- ^Sends a requests and retrieves answers from a Slim backend
    fetchAnswers :: st -> String -> Slim m st String
    -- ^Start slim backend from given initial state
    doStartSlim  :: st -> Slim m st ()
    -- ^Terminate slim backend with possibly some exit value
    ioTerminate  :: st -> Slim m st ExitCode

newtype Slim m st a = Slim { runS :: StateT st m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState st)

runSlim :: (SlimIO m st) => Slim m st a -> st -> m a
runSlim = evalStateT . runS

doSlim :: (SlimIO m st) => [ Instruction String ] -> Slim m st [( Instruction String, Answer)]
doSlim insts = let insts' = renumber insts
               in slimClient insts'  >>= return . matchQandA insts'

-- |Slim server does not seem to be usable after first run
doSendSlim :: (SlimIO m st) => [ Instruction String ] -> Slim m st [( Instruction String, Answer)]
doSendSlim insts = let insts' = renumber insts
                   in sendSlim insts'  >>= return . matchQandA insts'

slimClient :: (SlimIO m st) => [ Instruction String ] -> Slim m st (Maybe Answer)
slimClient call = do startSlim
                     answer <- sendSlim call
                     endSlim
                     return answer

endSlim :: (SlimIO m st) => Slim m st ExitCode
endSlim = get >>= ioTerminate

startSlim :: (SlimIO m st) => Slim m st ()
startSlim = do st <- get
               doStartSlim st

sendSlim :: (SlimIO m st) => [ Instruction  String ] -> Slim m st (Maybe Answer)
sendSlim insts  =
      do let msgs = encode insts
         slim <- get
         answer <- fetchAnswers slim msgs
         return $ (decode answer :: Maybe Answer)

data SlimConfig = SlimConfig {slimport       :: Integer,    -- ^The port this slim instance shall listen on
                              slimexecutable :: FilePath,   -- ^the Slim executable (eg. fitnesse.jar)
                              slimclasspath  :: [FilePath], -- ^
                              executiondir   :: FilePath,
                              verbose        :: Bool
                             }
                  deriving (Eq, Show, Read)

-- | Default slim configuration.
-- Assumes fitnesse's jar is in the execution directory of the process
defaultConfig = SlimConfig 0x8888 "java" [ "fitnesse.jar" ] "."  False
