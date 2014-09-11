{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Login.Login where
import qualified IOAutomaton.IOAutomaton as A

data LoginStatus = Logged
                | Unlogged 
                | Sink
                   deriving (Eq, Show)

data LoginInput = Enter Name Password
                | Logout
                  deriving (Eq, Show)

data LoginOutput = LoginFailed
                 | Ok
                   deriving (Eq, Show)

newtype Directory = Dir [(Name,Password)]
    deriving (Eq, Show) 

emptyDir = Dir []

type Name     = String
type Password = String 

data LoginState = L { status    :: LoginStatus,
                      directory :: Directory}
                  deriving (Eq, Show)

initLogin = L Unlogged emptyDir

type Transition = (LoginStatus, LoginInput, LoginOutput, LoginStatus)
    
type Path  = [ Transition ]
    
login ::  Name -> Password -> LoginState -> (Maybe LoginOutput, LoginState)
login n p l@(L Unlogged (Dir directory))  | (n,p) `elem` directory = (Just Ok, l { status = Logged })
                                          | otherwise              = (Just LoginFailed, l)
login _ _    l                                                     = (Nothing, l { status = Sink} )

logout :: LoginState -> (Maybe LoginOutput, LoginState)
logout l@(L Logged _) = (Just Ok, l { status = Unlogged })
logout l = (Nothing, l)

instance A.IOAutomaton LoginState LoginStatus LoginInput LoginOutput where
  init           = initLogin
  sink    _      = Sink
  action         = action
  state  (L s _) = s
  update a q     = a { status = q }

action :: LoginInput -> LoginState -> (Maybe LoginOutput, LoginState)
action a@(Enter n p) = login n p
action Logout        = logout
