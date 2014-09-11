module MarginRunGen where
import MarginRun
import Test.QuickCheck
import Control.Monad
import Control.Applicative

type Action = (MRInput, MROutput, MRState)

instance Arbitrary MarginRunTrace where
  arbitrary = liftM T (sized (automaton Init))
      
makeTrans st (i,o,f) = (st, i, o , f)

-- |Given a state and a list of possible actions, generate a transition
oneTransitionFrom :: MRState -> [ Gen Action ] -> Gen Trans
oneTransitionFrom startState delta = 
  oneof (liftM (makeTrans startState) <$> delta)

someTrans :: MRState -> Gen Trans
someTrans Init = oneTransitionFrom Init [
                         return (MRStart 1, MRStarted 1, Running)
                        ]
someTrans Running = oneTransitionFrom Running [
                         return (MRComplete, MRCompleted 1, Completed )
                        ]

someTrans Completed = oneTransitionFrom Completed [
                         return (MRApprove, MRApproved 1, Approved ),
                         return (MRReject, MRRejected 1, Rejected )
                        ]

someTrans Approved = oneTransitionFrom Approved [
                         return (MRClose, MRClosed 1, Closed )
                        ]


automaton st 0 = return []   
automaton st n = do t@(_,_,_,f) <- someTrans st
                    rest <- automaton f (n-1)
                    return $ (t : rest)
