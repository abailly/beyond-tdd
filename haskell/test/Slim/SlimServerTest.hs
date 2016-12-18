{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Slim.SlimServerTest where

import           Test.HUnit

import           Control.Concurrent  (killThread, threadDelay)
import           Control.Exception
import           Control.Monad.State
import qualified Data.Map            as M
import           Slim

allTests :: Test
allTests = TestList [
  canCallFunctionsFromSlimMessage,
  canCallFunctionsFromSlimMessage,
  returnAnExceptionString
  ]

canCallFunctionsFromSlimMessage :: Test
canCallFunctionsFromSlimMessage =
   "call one or more basic functions on current data state" ~:
   TestList [
        runState (call $ defineNumerator "10" ) divisionFixture ~?= ((A $ L [ S "2", S "/__VOID__/"]), Div 10 0),
        runState (call $ defineNumerator "5" ) divisionFixture ~?= ((A $ L [ S "2", S "/__VOID__/"]), Div 5 0),
        runState (call $ defineDenominator "2" ) divisionFixture ~?= ((A $ L [ S "2", S "/__VOID__/"]), Div 0 2),
        runState (call quotient' ) divisionFixture' ~?= ((A $ L [ S "4", S "5.0"]), Div 10 2),
        runState (call factory ) divisionFixture' ~?= ((A $ L [ S"1", S "OK"]), Div 0 0),
        testDivisionInput  `invoke`  divisionFixture ~?= testDivisionOutput
        ]

canCallCallFunctionsOnTwoDifferentInstances :: Test
canCallCallFunctionsOnTwoDifferentInstances =
   "can call mixed functions on 2 instances of the same 'class'" ~: TestList [
     runState (call factory) nothing ~?= ((A $ L [ S"1", S "OK"]), M.singleton "division" (Div 0 0)),
     execState (sequence $ map call mixedCalls) nothing ~?=   M.fromList [("division1",(Div 0 0)),
                                                                          ("division2",(Div 10 0))],
     testDivisionInput  `invoke`  nothing ~?= testDivisionOutput
     ]

returnAnExceptionString :: Test
returnAnExceptionString =
  "exceptions are returned upon failures to invoke method" ~: TestList [
    evalState (sequence $ map call callNonExistentMethod) nothing ~?=  map A (answersWith nonExistentError) ,
    callNonExistentMethod `invoke`  nothing ~?=  A (L $ answersWith nonExistentError),
    callNonExistentInstance `invoke`  nothing ~?=  A (L $ answersWith noInstanceError)
    ]

canConnectToSlimServerAndInvokeMethods :: Test
canConnectToSlimServerAndInvokeMethods =
  "start & connect to slim server, then send commands" ~:
  do tid <- startSlimServer port
     (liftIO $ threadDelay 500000)
     answer <- sendToSlimAndClose port divisionBy4 `catch` \ (e :: SomeException) -> return (KO (show e))
     print answer
     killThread tid
     assertEqual "output from Slim server is not correct" divisionBy4result answer
    where
      port = 4567

-- Test fixtures start here
testDivisionInput :: [Instruction [Char]]
testDivisionInput = [
 Make "1" "division" "eg.Division" [],
 Call "2" "division" "setNumerator" [ "10" ],
 Call "3" "division" "setDenominator" [ "2" ],
 Call "4" "division" "quotient" []
 ]

divisionBy4 :: [Instruction [Char]]
divisionBy4 = [
 Make "1" "division" "eg.Division" [],
 Call "2" "division" "setNumerator" [ "10" ],
 Call "3" "division" "setDenominator" [ "4" ],
 Call "4" "division" "quotient" []
 ]
divisionBy4result :: Answer
divisionBy4result = A $ L [L [S "1",S "OK"],L [S "2",S "/__VOID__/"],L [S "3",S "/__VOID__/"],L [S "4",S "2.5"]]

testDivisionOutput :: Answer
testDivisionOutput = A $ L [L [S "1",S "OK"],L [S "2",S "/__VOID__/"],L [S "3",S "/__VOID__/"],L [S "4",S "5.0"]]
divisionFixture :: Div
divisionFixture = Div 0 0
divisionFixture' :: Div
divisionFixture' = Div 10 2
quotient' :: Instruction [Char]
quotient' = Call "4" "division" "quotient" []
factory :: Instruction [Char]
factory =  Make "1" "division" "eg.Division" []
defineNumerator :: Argument -> Instruction [Char]
defineNumerator s = (Call "2" "division" "setNumerator" [ s ])
defineDenominator :: Argument -> Instruction [Char]
defineDenominator s = (Call "2" "division" "setDenominator" [ s ])
mixedCalls :: [Instruction [Char]]
mixedCalls = [
       Make "1" "division1" "eg.Division" [],
       Make "2" "division2" "eg.Division" [],
       Call "3" "division2" "setNumerator" [ "10" ]
       ]
nothing :: M.Map String Div
nothing = M.empty :: M.Map String Div

callNonExistentMethod :: [Instruction [Char]]
callNonExistentMethod = (mixedCalls ++ [  Call "4" "division2" "nonExistantMethod" [ "10" ] ])
callNonExistentInstance :: [Instruction [Char]]
callNonExistentInstance = (mixedCalls ++ [  Call "4" "division3" "setNumerator" [ "10" ] ])
nonExistentError :: NestedList
nonExistentError =  (L [ S "4", S "__EXCEPTION__: NO_METHOD_IN_CLASS nonExistantMethod division2"])
noInstanceError :: NestedList
noInstanceError =  (L [ S "4", S "__EXCEPTION__: NO_INSTANCE division3"])
answersWith :: NestedList -> [NestedList]
answersWith err      = [ (L [S "1",S "OK"]), (L [S "2",S "OK"]), (L [S "3",S "/__VOID__/"])] ++ [err]
