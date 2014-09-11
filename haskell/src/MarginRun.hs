module MarginRun where

data MRState = Init
             | Running
             | Completed
             | Approved
             | Closed
             | Rejected
             | Sink  -- ^Failure state, represents an incorrect trace
             deriving (Eq, Show,Ord)
                        
data MR = MR { mrid :: Integer
              , mrstate :: MRState 
              , dealScope :: DealScope }
          deriving (Eq, Show, Ord)
                   
data MROutput = MRStarted Integer
              | MRCompleted Integer
              | MRApproved Integer
              | MRClosed Integer
              | MRRejected Integer
              deriving (Eq, Show,Ord)

data MRInput = MRStart Integer
             | MRComplete
             | MRApprove
             | MRReject
             | MRClose
             deriving (Eq, Show, Ord)
                      
data DSState = Parked
             | DSRunning
             | DSRelaxed
             | DSRegistered
             deriving (Eq, Show, Ord)
               
data DealScope = DealScope { dsid :: Integer
                           , dsstate :: DSState }
             deriving (Eq, Show, Ord)
                      
type Trans = (MRState, MRInput, MROutput, MRState)

newtype MarginRunTrace = T [Trans] 
                       deriving (Show, Eq)
                                
goto :: MR -> (Integer -> a, MRState, DSState) -> (Maybe a, MR)
goto mr (o, s, dss) = (Just (o (mrid mr)), mr { mrstate = s, dealScope = (dealScope mr) {dsstate = dss} })
                          
start :: MR -> (Maybe MROutput, MR)

complete :: MR -> (Maybe MROutput, MR)

start     mr@(MR _ Init _ ) = mr `goto` (MRStarted, Running, DSRunning )
complete  mr                = mr `goto` (MRCompleted, Completed, DSRelaxed )
approve   mr                = mr `goto` (MRApproved, Approved, DSRelaxed )
close     mr                = mr `goto` (MRClosed, Closed, DSRegistered )
reject    mr@(MR _ Init _ ) = (Nothing, mr)
reject    mr                = mr `goto` (MRRejected, Rejected, Parked )

initializeMarginRun :: Integer -> MR
initializeMarginRun x = MR { mrid = x
                           , mrstate = Init 
                           , dealScope = DealScope { dsid = x, dsstate = Parked }
                           }

evalMR :: MR -> MarginRunTrace -> MR
evalMR _ _ = initializeMarginRun 1