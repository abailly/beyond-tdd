import ATMTest

import IOAutomaton
import ATM hiding (verbose)
import Slim
import SlimAutomaton
import ATMSlimTest
import Slim.SlimTest

import qualified Data.Map as M

import qualified Slim.SlimServerTest as Server
import Test.HUnit hiding (Path,assert)
import Test.QuickCheck.Monadic
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import System.IO.Unsafe

import System.Environment(getArgs)
import System.IO(stderr)

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts


runUnitTests = runTest $ TestList [
		atmTransitionsUpdateState
               ,testATMMachineValidator
               ,testATMStateT
                ,testATMSlimProtocol
                ,testATMOracle
                 ,Server.allTests
--                ,TestList [
-- 		 canEncodeNestedLists
--                 ,canEncodeInstructions
--                 ,canDecodeAnswer
-- --                ,slimClientSendAndReceiveMessages
--                 ,testCanRenumberInstructions
--                 ,matchInstructionsToAnswers
-- 	        ]
	       ]

main = do args <- getArgs 
          let debugSlim = checkArgs args
          quickCheck (monadic (unsafePerformIO) (atmRespectsItsModel debugSlim))  >> runUnitTests

checkArgs :: [ String ] -> Bool
checkArgs [ "-v" ]  = True
checkArgs _         = False

myBank = Bank $ M.fromList [(Acc "123456",100),(Acc "234567",1000),(Acc "567890",12345)]
initAtm' = initAtm {bank = myBank }


-- | Returns empty machine if cannot generate a meaningful one
validMachines :: Gen ATMMachine
validMachines = do m  <- arbitrary
                   let T m' = evalTrace m initAtm'
                   return (if isValidPath m' then
                               T m'
                           else
                               T [])
                         
                 
atmRespectsItsModel :: Bool -> PropertyM IO ()
atmRespectsItsModel debug = 
  forAllM validMachines $ \m ->
    do pre $ nonEmpty m
       run $ putStrLn $ "checking ATM with trace " ++ (show m)
       b' <- run $ runSlim (do startSlim
                               b <- testSUT initAtm' m
                               endSlim
                               return b
                           ) defaultSlim { config = defaultConfig { slimclasspath = [ "fitnesse.jar", "atm.jar" ] , verbose = debug } }
       run $ putStrLn $ case b' of 
         Nothing -> "Success"
         _       -> "Failure at "++ (show b')
       assert (b' == Nothing)
    where
      nonEmpty (T []) = False
      nonEmpty _      = True
