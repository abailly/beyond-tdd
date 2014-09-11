{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IOAutomaton.Generator where
import Test.QuickCheck
import qualified IOAutomaton.IOAutomaton as A
import Data.Maybe(fromJust)
import Control.Monad 

-- |Apply a given function over all elements of a list and select one of the
-- results.
selector :: (Arbitrary b) => [a] -> (a -> b) -> Gen b
selector list ctor = oneof (map (return . ctor) list)

-- |Specific IOAutomaton models provide instances of this class to generate 
-- 'valid' traces.
class (Eq q) => 
      TransitionGenerator q i o  | q -> i o where
  -- ^ Generate one transition from the given state
  someTransition :: q -> Gen (q, i, o, q)
  someTransition st = sequence stateMachine >>= (oneof . (map return) . (filter $ sameState st))
    where 
      sameState :: (Eq q) => q -> (q, i, o, q) -> Bool
      sameState st (s,i,o,e) = st == s
  -- ^ Provide all transitions for a given model  
  stateMachine :: [ Gen (q,i,o,q)]
  -- ^ Provide starting state of the automaton
  startState     :: q
  transitions    :: q -> Int -> Gen [ (q,i,o,q) ]
  transitions st 0 = return []
  transitions st n = do t@(_,_,_,st') <- someTransition st
                        rest <- transitions st' (n-1)
                        return $ (t : rest)

-- | Given a transition generator and some atomic components generators, 
-- we can define an arbitrary trace instance.
instance (TransitionGenerator q i o) 
         => Arbitrary (A.Trace q i o) where
  arbitrary = liftM A.T (sized (transitions startState))
  
-- |Generate valid traces to stdout.
-- This function is mainly used for testing purpose as it allows one to easily 
-- generates a bunch of traces for a given model.
generate :: (A.IOAutomaton a q i o,
             TransitionGenerator q i o) => a -> IO ()
generate init = do candidates <- (sample' arbitrary)
                   putStrLn $ "generating " ++ show (length candidates) ++ " candidates"
                   let accepted = filter (select init) candidates
                   putStrLn $ "selecting " ++ show (length accepted) ++ " traces"
                   sequence $ map (putStrLn . show) accepted
                   return ()
                where
                  select :: (A.IOAutomaton a q i o) => a -> A.Trace q i o -> Bool
                  select st a = (fst $ fromJust $ A.runAutomaton a st) /= Nothing

