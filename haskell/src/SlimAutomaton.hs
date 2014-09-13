{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |Bridge from IOautomata to Slim.
-- This module contains bridge functions that ease the execution of tests
-- from a given IOAutomaton model through Slim protocol.
module SlimAutomaton(SlimAutomaton(..),
                     testSUT,
                     checkSUT,
                     module IOAutomaton,
                     module Slim,
                     module Slim)
       where

import IOAutomaton
import Slim

class (IOAutomaton a q i o) => SlimAutomaton a q i o | a -> q i o where
  actionTrans :: a -> i -> Instruction String
  start       :: a -> [ Instruction String ]
  interpret   :: a -> i -> Answer -> Maybe o


testTrans :: (SlimAutomaton a q i o) => a -> (q, i, o ,q) -> Instruction String
testTrans a (s, i, o, e) = actionTrans a i

testSUT :: (SlimAutomaton a q i o, SlimIO m st) => a -> Trace q i o -> Slim m st (Maybe  ( (q, i, o ,q),Maybe o))
testSUT a t@(T trans) = do let insts = map (testTrans a) trans
                           resps <- doSendSlim $ start a ++ insts
                           let b = checkSUT a trans (drop 4 resps)
                           return b

-- | Main check function returns failing instruction or Nothing if SUT passes
-- check
-- Assumes each transition matches one pair of instruction/answer
checkSUT :: (SlimAutomaton a q i o) => a -> [ (q, i, o ,q) ] -> [( Instruction String, Answer)] -> Maybe ((q,i,o,q),Maybe o)
checkSUT _ [] []                           = Nothing
checkSUT at (t'@(s,i,o,e):trans) ((_,a):resps) =
    case interpreted of
      Just t  -> if t == o then
                     checkSUT (update at e) trans resps
                 else
                     Just (t',Just t)
      Nothing -> Just (t',Nothing )
  where
    interpreted = interpret at i a
