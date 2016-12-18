{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ATMTest where

import Test.HUnit
import IOAutomaton hiding (state)

import Control.Monad.Writer
import Control.Monad.State hiding (state)

import qualified Data.Map as M

import ATM
    
atmTransitionsUpdateState :: Test
atmTransitionsUpdateState = TestList [
                             (snd $ (enterCard myCard) initAtm)                                 ~?= Atm EnteringPin (Just myCard) (Bank M.empty)
                            ,(fst $ (enterCard myCard) (waitingForPin))                         ~?= Nothing
                            ,enterCard (myCard'')  initAtm                                      ~?= (Just CardRetained, Atm Init Nothing (bank initAtm))
                            ,enterPin (Pin "1234")  waitingForPin                               ~?= (Just OK, Atm SelectingAction (Just myCard) myBank)
                            ,enterPin (Pin "2345")  waitingForPin                               ~?= (Just FailedCode, Atm EnteringPin (Just myCard')  myBank)
                            ,enterPin (Pin "2345") (waitingForPin    {card = Just myCard''})    ~?= (Just CardRetained, Atm Init Nothing myBank)
                            ,enterPin (Pin "2345") (waitingForPin    {state = Init})            ~?= (Nothing, Atm Sink (Just myCard) myBank)
                            ,withdrawMoney         (waitingForPin    {state = SelectingAction}) ~?= (Just OK, Atm SelectingAmount (Just myCard) myBank)
                            ,withdrawMoney         (waitingForPin    {state = Init})            ~?= (Nothing, Atm Sink (Just myCard) myBank)
                            ,enterAmount 100        waitingForAmount                            ~?= (Just DeliverNotes, Atm SelectingAction (Just myCard) myBank')
                            ,enterAmount 100       (waitingForAction {bank = myBank'})          ~?= (Just NotEnoughBalance, Atm SelectingAction (Just myCard) myBank')
                            ,getBalance            (waitingForAction {bank = myBank'})          ~?= (Just$  Bal 0, Atm SelectingAction (Just myCard) myBank')
                            ,getBalance             waitingForAction                            ~?= (Just$  Bal 100, Atm SelectingAction (Just myCard) myBank)
                            ,getBalance            (waitingForAmount {card = Just otherCard})   ~?= (Nothing, Atm Sink (Just otherCard) myBank)
                            ,exit                   waitingForAmount                            ~?= (Just Bye, Atm Init Nothing myBank)
                            ]
    where
      myCard            = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }
      otherCard         = Card { pin = Pin "1234" , accountNo = Acc "234567", failedCode = 0 }
      myCard'           = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 1 }
      myCard''          = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 2 }
      waitingForPin     = Atm EnteringPin (Just myCard) myBank
      waitingForAmount  = waitingForPin { state = SelectingAmount }
      waitingForAction  = waitingForPin { state = SelectingAction }
      myBank            = Bank $ M.singleton (Acc "123456") 100
      myBank'           = Bank $ M.singleton (Acc "123456") 0

testATMMachineValidator :: Test
testATMMachineValidator = "ATM validator" ~: TestList [
                           "check empty trace " ~: (runAutomaton (T []) initAtm) >>= (assertEqual "incorrect state" (Nothing, initAtm))
                          ,runAutomaton (T [ (Init, EnterCard card, OK, EnteringPin )]) initAtm ~?= Just (Just OK,initAtm {state = EnteringPin, card = Just card})
                          ,runAutomaton (T [(Init,EnterCard card,OK,EnteringPin),(EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin),(EnteringPin,EnterPinCode (Pin "1234"),OK,SelectingAction)]) initAtm ~?= Just (Just OK, initAtm { state = SelectingAction, card = Just (card {failedCode = 1})})
                          ]
    where
      card = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }

testATMStateT :: Test
testATMStateT = "ATM State transformer" ~:
                TestList [
                  "test valid sequence" ~: runStateT actions initAtm >>=
                                            (assertEqual "state is SelectingAction" SelectingAction) . (state . snd)
                , "test invalid sequence" ~: runStateT failureActions initAtm >>=
                                              (assertEqual "state is Sink" Sink) . (state . snd)
                , "test evalATM" ~:
                evalState actionsWithExit initAtm ~?=
                              [(Init,EnterCard (Card {pin = Pin "1234", accountNo = Acc "234567", failedCode = 1}),OK,EnteringPin)
                              ,(EnteringPin,Exit,Bye,Init)
                              ,(Init,EnterCard (Card {pin = Pin "2345", accountNo = Acc "123456", failedCode = 2}),CardRetained,Init)]
                ]
                     where
                       actionsWithExit = sequence $ map (evalATM) ([(Init,EnterCard (Card {pin = Pin "1234", accountNo = Acc "234567", failedCode = 1}),OK,EnteringPin)
                                                                      ,(EnteringPin,Exit,OK,Init)
                                                                      ,(Init,EnterCard (Card {pin = Pin "2345", accountNo = Acc "123456", failedCode = 2}),CardRetained,Init)])
                       actions = sequence $ map (runAndShow) ([(Init,EnterCard card,OK,EnteringPin)
                                                              ,(EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin)
                                                              ,(EnteringPin,EnterPinCode (Pin "1234"),OK,SelectingAction)])
                       failureActions = sequence $ map (runAndShow) ([(Init,EnterCard card,OK,EnteringPin)
                                                                     ,(EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin)
                                                                     ,(EnteringPin,GetBalance,FailedCode,Sink)])
                       card = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }
                       runAndShow  tr = do t <- evalST tr
                                           s <- get
                                           lift (putStrLn $ show s)
                                           return t
                                           
