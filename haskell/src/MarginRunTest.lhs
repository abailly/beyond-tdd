First a bunch of imports...

> import Test.HUnit
> import Test.QuickCheck
> import qualified Data.Map as M
> import IO (stderr)
> import System.Random
> import System.Exit
> import MarginRun
> import MarginRunGen
> import Data.List
>     

We define some utility operators to have more expressive tests, in 
BDD style reminiscent of what is done in a framework like specs.

> should n = (n ~:) .  TestList  
> for    n = (n ~:)
> infixl 0 `for`

Then we define some objects that our test will work on: Margin Runs 
in various states, deal scopes and utility functions to express 
transitions from one state to another.

> 
> outputAndGoto out = (,) ((Just . ($1)) out)
> outputNothingAndGoto st = (,) Nothing st
> 
> 
> scope1          = DealScope 1 Parked
> runningscope    = DealScope 1 DSRunning
> relaxedscope    = DealScope 1 DSRelaxed
> registeredscope = DealScope 1 DSRegistered
> initMR          = MR {mrstate = Init      , mrid = 1, dealScope = scope1 }
> runningMR       = MR {mrstate = Running   , mrid = 1, dealScope = runningscope }
> completedMR     = MR {mrstate = Completed , mrid = 1, dealScope = relaxedscope }
> approvedMR      = MR {mrstate = Approved  , mrid = 1, dealScope = relaxedscope }
> closedMR        = MR {mrstate = Closed    , mrid = 1, dealScope = registeredscope }
> rejectedMR      = MR {mrstate = Rejected  , mrid = 1, dealScope = scope1 }
> 

Here come some unit tests for the action functions of MarginRun, 
expressing some modification of the state of a MR.

> tests = test [
>   "a margin run" `should` [
>      "move to running and move DS to running when started" `for`
>      start initMR        ~?= outputAndGoto MRStarted runningMR
>      
>      ,"move to completed and move DS to relaxed upon completion" `for`
>      complete runningMR  ~?= outputAndGoto MRCompleted completedMR
>      
>      ,"move to approved upon receiving approval" `for`
>      approve completedMR ~?= outputAndGoto MRApproved approvedMR
>      
>      ,"move to closed and move DS to registered upon closure" `for`
>      close approvedMR    ~?= outputAndGoto MRClosed closedMR
>      
>      ,"move to rejected and move DS to parked upon rejection (from running)" `for`
>      reject runningMR    ~?= outputAndGoto MRRejected rejectedMR
>      
>      ,"not move upon rejection from start" `for`
>      reject initMR       ~?= outputNothingAndGoto initMR
>      ],
>

This unit tests serves only as a marker to show we can generate 
instances of MarginRunTrace using QuickCheck framework.
   
>   "a margin run generator" `should` [
>     "produce no transition" `for`
>     sample' arbitrary >>= return . head 
>       >>= (assertEqual "incorrect transition generated" (T []))
>     ]
>   ]
> 

Then we test the QuickCheck generator to ensure that only legal 
sequences of transitions can be generated. This state should fail 
intentionally until the evalMR function is complete.

> prop_produceOnlyLegalSequenceOfActions :: MarginRunTrace -> Bool
> prop_produceOnlyLegalSequenceOfActions mr = 
>   mrstate (evalMR initMR mr) /= Running
>   

Special Test running function to better integrate with Emacs and 
produce a meaningful error code upon exit.

> main = do counts <- runTest tests
>           qc <- quickCheckResult prop_produceOnlyLegalSequenceOfActions
>           case qc of
>             Failure _ _ r _ -> print r
>             _               -> return ()
>           case (errors counts + failures counts) of
>             0 -> exitWith ExitSuccess
>             n -> exitWith (ExitFailure n)
> 
> runTest :: Test -> IO Counts
> runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
>    	          return counts
>