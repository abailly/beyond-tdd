{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.HUnit
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Test.QuickCheck
import qualified Data.Map as M
import IO (stderr)
import System.Random
import System.Exit
import MarginRun
import MarginRunGen
import Data.List
    
should n = (n ~:) .  TestList  
for    n = (n ~:)

outputAndGoto out = (,) ((Just . ($1)) out)
outputNothingAndGoto st = (,) Nothing st

infixl 0 `for`

scope1          = DealScope 1 Parked
runningscope    = DealScope 1 DSRunning
relaxedscope    = DealScope 1 DSRelaxed
registeredscope = DealScope 1 DSRegistered
initMR          = MR {mrstate = Init      , mrid = 1, dealScope = scope1 }
runningMR       = MR {mrstate = Running   , mrid = 1, dealScope = runningscope }
completedMR     = MR {mrstate = Completed , mrid = 1, dealScope = relaxedscope }
approvedMR      = MR {mrstate = Approved  , mrid = 1, dealScope = relaxedscope }
closedMR        = MR {mrstate = Closed    , mrid = 1, dealScope = registeredscope }
rejectedMR      = MR {mrstate = Rejected  , mrid = 1, dealScope = scope1 }

tests = test [
  "a margin run" `should` [
     "move to running and move DS to running when started" `for`
     start initMR        ~?= outputAndGoto MRStarted runningMR
     
     ,"move to completed and move DS to relaxed upon completion" `for`
     complete runningMR  ~?= outputAndGoto MRCompleted completedMR
     
     ,"move to approved upon receiving approval" `for`
     approve completedMR ~?= outputAndGoto MRApproved approvedMR
     
     ,"move to closed and move DS to registered upon closure" `for`
     close approvedMR    ~?= outputAndGoto MRClosed closedMR
     
     ,"move to rejected and move DS to parked upon rejection (from running)" `for`
     reject runningMR    ~?= outputAndGoto MRRejected rejectedMR
     
     ,"not move upon rejection from start" `for`
     reject initMR       ~?= outputNothingAndGoto initMR
     ],
  
  "a margin run generator" `should` [
    "produce no transition" `for`
    sample' arbitrary >>= return . head 
      >>= (assertEqual "incorrect transition generated" (T []))
    ]
  ]

prop_produceOnlyLegalSequenceOfActions :: MarginRunTrace -> Bool
prop_produceOnlyLegalSequenceOfActions mr = 
  mrstate (evalMR initMR mr) /= Sink
  
main = do counts <- runTest tests
          qc <- quickCheckResult prop_produceOnlyLegalSequenceOfActions
          case qc of
            Failure _ _ r _ -> print r
            _               -> return ()
          case (errors counts + failures counts) of
            0 -> exitWith ExitSuccess
            n -> exitWith (ExitFailure n)

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts
