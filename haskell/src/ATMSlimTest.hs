{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ATMSlimTest(testATMSlimProtocol
                  ,testATMOracle
                  ,module Test.HUnit
                  ,module ATM.ATM
                  ,module ATM.ATMSlim)
    where
import Slim.SlimClient
import Slim.SlimClientIO
import SlimAutomaton.SlimAutomaton
import Test.HUnit hiding (Path,assert)
import Control.Applicative
import Control.Monad.Writer
import qualified Data.Map as M
import System.Random
import Slim.Slim
import ATM.ATM
import ATM.ATMSlim
import Data.List
    
evalActions :: [ Instruction String ] -> IO [(Instruction String, Answer)]
evalActions actions = runSlim (doSlim actions) slimAtm
    where
      slimAtm   = defaultSlim { config = defaultConfig { slimclasspath = [ "fitnesse.jar", "atm.jar" ] , verbose = True } }

testATMSlimProtocol = "ATM <-> Slim protocol tests" ~:
                      TestList [
                          "check Creation" ~: evalActions  [startATM] 
                                       >>= 
                          (assertEqual "create fixture" [
                                        (Make "1" "atm" "oqube.dab.ATMFixture" [], A $ S "OK")])
                         ,"check Card insertion" ~:  evalActions
                                      [startATM, actionTrans atm (EnterCard card)] >>= 
                          (assertEqual "create fixture"  [
                                        (Make "1" "atm" "oqube.dab.ATMFixture" [], A $ S "OK")
                                       ,(Call "2" "atm" "insertCard" [pinC, acc, show $ failures], A $ S "/__VOID__/")
                                           ])
                         ,"check Card insertion with error" ~:  evalActions
                                      [startATM, actionTrans atm (EnterCard card { failedCode = failures'}) ] >>= 
                         (assertThrowsException "oqube.dab.BankException")
                         ,"check correct pincode" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "1234")) ]) >>= 
                         (assertResultStringMatch "true")
                         ,"check incorrect pincode" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "2345")) ]) >>= 
                         (assertResultStringMatch "false")
                         ,"check card is retained after 4 failed attempts" ~:  evalActions
                                     ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "2345")), actionTrans atm (EnterPinCode  (Pin "2345")), actionTrans atm (EnterPinCode  (Pin "2345")), actionTrans atm (EnterPinCode  (Pin "2345")) ]) >>= 
                         (assertThrowsException "oqube.dab.BankException")
                         ,"check get balance" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "1234")), actionTrans atm GetBalance  ]) >>= 
                         (assertResultStringMatch "150")
                         ,"check cash withdrawal w/ enough balance" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "1234")), actionTrans atm (WithdrawMoney) , actionTrans atm (EnterAmount 100) ]) >>= 
                         (assertResultStringMatch "/__VOID__/")
                         ,"check cash withdrawal w/o enough balance throws exception" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "1234")), actionTrans atm (WithdrawMoney) , actionTrans atm (EnterAmount 1000) ]) >>= 
                         (assertThrowsException "oqube.dab.BankException")
                         ,"check get balance w/o card throws exception" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm GetBalance  ]) >>= 
                         (assertThrowsException "oqube.dab.CardInvalidException")
                         ,"check exiting from system" ~:  evalActions
                                      ((startATMWith atm) ++ [ actionTrans atm (EnterCard card), actionTrans atm (EnterPinCode  (Pin "1234")), actionTrans atm Exit ]) >>= 
                         (assertResultStringMatch "/__VOID__/")
                      ]

    where
      assertThrowsException :: String -> [ (Instruction String, Answer) ] -> Assertion
      assertThrowsException exc []                = return ()
      assertThrowsException exc [x@(i,A (S res))] = putStrLn (show x) >>
                                                    if isPrefixOf ("__EXCEPTION__:" ++ exc) res then
                                                        return ()
                                                    else
                                                        fail $ "Exception \"" ++ exc ++ "\" not last answer of trace"
      assertThrowsException exc (x:xs)            = putStrLn (show x) >>
                                                    assertThrowsException exc xs

      assertResultStringMatch :: String -> [ (Instruction String, Answer) ] -> Assertion
      assertResultStringMatch res [x@(i,A (S res'))]
                    | res == res'                  = putStrLn (show x)
      assertResultStringMatch res (x:xs)           = putStrLn (show x) >> assertResultStringMatch res xs
      assertResultStringMatch res xs               = fail $ "No matching result for "++ res ++ " in " ++ (show xs)
      pinC      = "1234"
      acc       = "123456"
      failures  = 0
      failures' = 3
      card      = Card { pin = Pin pinC , accountNo = Acc acc, failedCode = failures }
      bank      = Bank $ M.singleton (Acc "123456") 150
      atm       = Atm Init Nothing bank



testATMOracle = "oracle for ATM output" ~:
                TestList [
                     interpret (Atm Init Nothing bank)            (EnterCard card) (A (S "/__VOID__/"))                                   ~?= Just OK
                    ,interpret (Atm Init Nothing bank)            (EnterCard card) (A (S "__EXCEPTION__:oqube.dab.BankException"))        ~?= Just CardRetained
                    ,interpret (Atm Init Nothing bank)            (EnterCard card) (A (S "__EXCEPTION__:oqube.dab.CardInvalidException")) ~?= Just CardRetained
                    ,interpret (Atm EnteringPin Nothing bank)     (EnterPinCode p) (A (S "true"))                                         ~?= Just OK
                    ,interpret (Atm EnteringPin Nothing bank)     (EnterPinCode p) (A (S "false"))                                        ~?= Just FailedCode
                    ,interpret (Atm EnteringPin Nothing bank)     (EnterPinCode p) (A (S "__EXCEPTION__:oqube.dab.BankException"))        ~?= Just CardRetained
                    ,interpret (Atm SelectingAction Nothing bank) (GetBalance)     (A (S "50"))                                           ~?= Just (Bal 50)
                    ,interpret (Atm SelectingAction Nothing bank) (Exit)           (A (S "/__VOID__/"))                                   ~?= Just Bye
                    ,interpret (Atm SelectingAction Nothing bank) (WithdrawMoney)  (A (S "/__VOID__/"))                                   ~?= Just OK
                    ,interpret (Atm SelectingAmount Nothing bank) (EnterAmount a)  (A (S "/__VOID__/"))                                   ~?= Just DeliverNotes
                    ,interpret (Atm SelectingAmount Nothing bank) (EnterAmount a)  (A (S "__EXCEPTION__:oqube.dab.BankException"))        ~?= Just NotEnoughBalance
                ]
    where
      card = Card { pin = p , accountNo = Acc "123456", failedCode = 0 }
      p    = Pin "1234"
      a    = 100
      bank      = Bank $ M.singleton (Acc "123456") 150
