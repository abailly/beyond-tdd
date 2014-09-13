{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ATM.ATMGen (module Test.QuickCheck)
where
import ATM.ATM
import IOAutomaton.Generator
import Test.QuickCheck
import Control.Monad
import Control.Applicative

numbers :: Int -> Gen String
numbers n = sequence (replicate n (choose ('0','9')))

instance Arbitrary Pincode where
    arbitrary = selector [ "1234", "2345", "5678"] Pin
                
instance Arbitrary AccountNo where
    arbitrary = selector [ "123456", "234567", "567890"] Acc
                
instance Arbitrary Card where
    arbitrary = liftM3 Card arbitrary arbitrary (choose (0, 2))

-- |Lift constructors for ATMInput that require an argument.
--someInput :: (Arbitrary t) => (t -> ATMInput) -> Gen ATMInput
someInput f = f <$> arbitrary
                    
someBalance :: Gen ATMOutput
someBalance = Bal <$> choose (0, 10000)

someAmount :: Gen ATMInput
someAmount = EnterAmount <$> oneof (map return [10, 20 .. 200 ])

instance TransitionGenerator ATMState ATMInput ATMOutput where
  startState   = Init
  stateMachine = [
     (\c -> (Init, c , OK , EnteringPin))                                 <$> someInput EnterCard
    ,(\p -> (EnteringPin, p   , OK         ,SelectingAction))             <$> someInput EnterPinCode
    ,(\p -> (EnteringPin, p   , FailedCode ,EnteringPin))                 <$> someInput EnterPinCode
    ,(\p -> (EnteringPin, p   , CardRetained ,Init))                      <$> someInput EnterPinCode
    ,return (EnteringPin, Exit, Bye          ,Init)
    ,(\b -> (SelectingAction, GetBalance   , b  , SelectingAction))       <$> someBalance
    ,return (SelectingAction, WithdrawMoney, OK , SelectingAmount)
    ,return (SelectingAction, Exit         , OK , Init)
    ,(\m -> (SelectingAmount, m    , DeliverNotes     , SelectingAction)) <$> someAmount
    ,(\m -> (SelectingAmount, m    , NotEnoughBalance , SelectingAction)) <$> someAmount
    ,return (SelectingAmount, Exit , OK               , SelectingAction)
    ]

