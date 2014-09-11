{-# LANGUAGE MultiParamTypeClasses #-}
module Login.LoginGen where

import qualified IOAutomaton.IOAutomaton as A
import qualified IOAutomaton.Generator as G
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Login.Login

users :: [(Name, Password)]
users = [ ("titi","toto"),
          ("tutu","tata"),
          ("chris", "markov"),
          ("dave","toto"),
          ("gus","robert"),
          ("chas", "binge"),
          ("bob","joke"),
          ("allison","tata"),
          ("batch", "key")
        ]

someUsers i = return $ take i users

passwords  = map snd users
names      = map fst users

somelogin :: Gen LoginInput
somelogin = liftM2 Enter (elements names) (elements passwords)

instance G.TransitionGenerator LoginStatus LoginInput LoginOutput where
  startState = Unlogged
  stateMachine = [  
    (\c -> (Unlogged, c, Ok         , Logged))  <$> somelogin,
    (\c -> (Unlogged, c, LoginFailed, Unlogged)) <$> somelogin,
    return (Logged, Logout, Ok, Unlogged)
    ]

genAutomaton :: LoginState -> IO ()
genAutomaton = G.generate
