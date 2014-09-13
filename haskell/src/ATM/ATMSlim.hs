{-# LANGUAGE MultiParamTypeClasses #-}
module ATM.ATMSlim (
                startATM
               ,startATMWith
               ,atmOutputInterpreter
                ,testSUT)
where
import ATM.ATM
import SlimAutomaton
import qualified Data.Map as M


instance SlimAutomaton AtmState ATMState ATMInput ATMOutput where
  start       = startATMWith
  actionTrans = inputToSlim
  interpret   = atmOutputInterpreter
    
startATM :: Instruction String
startATM = Make "" "atm" "oqube.dab.ATMFixture" []

startATMWith :: AtmState -> [Instruction String]
startATMWith (Atm _ _ (Bank b)) = [ Make "" "atm" "oqube.dab.ATMFixture" [] ] ++
                                  (map initAccount (M.toList b))
  where
    initAccount (Acc acc, bal) = (Call "" "atm" "setAccountBalance" [ acc, show bal])


inputToSlim :: AtmState -> ATMInput -> Instruction String
inputToSlim _ (EnterCard (Card (Pin p) (Acc acc) (failures))) = Call "12" "atm" "insertCard" [ p,acc, show $ failures]
inputToSlim _ (EnterPinCode (Pin p))                          = Call "" "atm" "enterPinCode" [ p ]
inputToSlim _ (EnterAmount amount)                            = Call "" "atm" "withdrawal" [ show amount ]
inputToSlim _ (WithdrawMoney)                                 = Call "" "atm" "selectWithdraw" []
inputToSlim _ (GetBalance)                                    = Call "" "atm" "getBalance" []
inputToSlim _ (Exit)                                          = Call "" "atm" "withdrawCard" []


-- | Transforms a low-level answer into a possible output
-- according to current state.
atmOutputInterpreter :: AtmState -> ATMInput -> Answer -> Maybe ATMOutput
atmOutputInterpreter (Atm Init _ _ ) (EnterCard _) a@(A (S msg)) | msg == "/__VOID__/"   = Just OK
                             | isException "oqube.dab.CardInvalidException" a = Just CardRetained
                             | isException "oqube.dab.BankException"        a = Just CardRetained
atmOutputInterpreter (Atm EnteringPin _ _) (EnterPinCode _) a@(A (S msg))
                             | msg == "true"                                  = Just OK
                             | msg == "false"                                 = Just FailedCode
                             | isException "oqube.dab.BankException"        a = Just CardRetained
atmOutputInterpreter (Atm SelectingAction  _ _) (GetBalance) (A (S msg)) = case maybeAnInt msg of
                                                         Just amount -> Just (Bal amount)
                                                         _           -> Nothing
                                                                        
atmOutputInterpreter (Atm SelectingAction _ _) (Exit) (A (S "/__VOID__/"))          = Just Bye
atmOutputInterpreter (Atm EnteringPin _ _)     (Exit) (A (S "/__VOID__/"))          = Just Bye
atmOutputInterpreter (Atm SelectingAmount _ _ ) (Exit) (A (S "/__VOID__/"))          = Just Bye
atmOutputInterpreter (Atm SelectingAction _  _) (WithdrawMoney) (A (S "/__VOID__/")) = Just OK
atmOutputInterpreter (Atm SelectingAmount _ _ ) (EnterAmount _) a@(A (S msg))
                             | msg == "/__VOID__/"                            = Just DeliverNotes
                             | isException "oqube.dab.BankException"        a = Just NotEnoughBalance
-- pretty much anything else is rubbish...
atmOutputInterpreter _ _                                                    _ = Nothing

