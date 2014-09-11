{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module Slim.SlimTest where

import Test.HUnit
import Test.QuickCheck
import Control.Monad(replicateM,foldM,liftM,liftM2,liftM4,liftM5)
import IO (stderr)
import Data.Maybe
import Slim.Slim
import Slim.SlimClient
import Slim.SlimClientIO

canEncodeNestedLists =
   TestList [
        (encode $ S "some.clas.path" )
            ~?= "000014:some.clas.path"
       ,(encode $ L [] )
            ~?= "000009:[000000:]"
       ,(encode $ L [S "some.clas.path"] )
            ~?= "000031:[000001:000014:some.clas.path:]"
       ,(encode $ L [S "some.clas.path", S "else"] )
            ~?= "000043:[000002:000014:some.clas.path:000004:else:]"
       ,(encode $ L [S "some.clas.path", L [S "else", S "other"]] )
            ~?= "000073:[000002:000014:some.clas.path:000034:[000002:000004:else:000005:other:]:]"
     ]


instance Arbitrary NestedList where
  arbitrary = frequency [
                    (3, liftM S arbitrary)
                   ,(1, liftM L arbitrary)
                  ]

instance Arbitrary Answer where
  arbitrary = oneof [
    liftM A arbitrary,
    liftM KO arbitrary
                    ]
              
tests_QuickCheck :: [IO Result]
tests_QuickCheck = [
                    quickCheckResult prop_decodeIsInverseToEncodeForNestedList
                   ]


instance Arbitrary (Instruction String) where
  arbitrary = oneof [
    liftM2 Import aNumber arbitrary
    ,liftM4 Make aNumber anInstance aClass arbitrary
    ,liftM4 Call aNumber anInstance aMethod arbitrary
    ,liftM5 CallAndAssign aNumber anInstance aSymbol aMethod arbitrary
                    ] 
    where
      aNumber = (arbitrary :: Gen Int) >>= (return . show)
      select = oneof . (map return)
      anInstance = select  ["a","b","c", "d"]
      aClass = select ["C1","C2","C3", "C4"]
      aMethod = select ["m1","m2","m3", "m4"]
      aSymbol = select ["x","y","z", "t"]

roundtrip :: (SlimEncodable a, SlimDecodable a, Eq a, Show a, Arbitrary a) => a -> a
roundtrip = (fromJust . decode . encode )

prop_decodeIsInverseToEncodeForNestedList :: NestedList -> Bool
prop_decodeIsInverseToEncodeForNestedList s  =
   (roundtrip s) == s

canEncodeInstructions =
   TestList [
        (encode $ Import "id_1" "some.clas.path" )
            ~?= "000057:[000003:000004:id_1:000006:import:000014:some.clas.path:]"
       ,(encode $ Make  "id_2" "toto" "some.Class" ["1", "arg"] )
            ~?= "000083:[000006:000004:id_2:000004:make:000004:toto:000010:some.Class:000001:1:000003:arg:]"
       ,(encode $ Call  "id_2" "toto" "someMethod" ["1", "arg", "12"] )
            ~?= "000093:[000007:000004:id_2:000004:call:000004:toto:000010:someMethod:000001:1:000003:arg:000002:12:]"
       ,(encode $ CallAndAssign  "id_2" "X" "toto" "someMethod" ["1", "arg",
   "12"] )
            ~?= "000111:[000008:000004:id_2:000013:callAndAssign:000001:X:000004:toto:000010:someMethod:000001:1:000003:arg:000002:12:]"
     ]

prop_decodeIsInverseToEncodeForListOfInstructions :: [ Instruction String ] -> Bool
prop_decodeIsInverseToEncodeForListOfInstructions s =  
  (roundtrip s ) == s
  
prop_decodeIsInverseToEncodeForAnswer :: Answer -> Bool
prop_decodeIsInverseToEncodeForAnswer s =  
  (roundtrip s ) == s
  
canDecodeAnswer =
   TestList [
        (decode $ "000002:OK" )
            ~?= Just (A $ S "OK")
      , (decode $ "000019:__EXCEPTION__:error" )
            ~?= Just (KO "error")
     ]

testDivisionInput = [
 Make "1" "division" "eg.Division" [],
 Call "2" "division" "setNumerator" [ "10" ],
 Call "3" "division" "setDenominator" [ "2" ],
 Call "4" "division" "quotient" []
 ]

testDivisionOutput = A $ L [L [S "1",S "OK"],L [S "2",S "/__VOID__/"],L [S "3",S "/__VOID__/"],L [S "4",S "5.0"]]

testRenumberingInput = [
 Make "0" "division" "eg.Division" [],
 Call "" "division" "setNumerator" [ "10" ],
 Import "31" "division",
 Call "3" "division" "setDenominator" [ "2" ],
 CallAndAssign "35" "division" "toto" "setDenominator" [ "2" ],
 Call "4" "division" "quotient" []
 ]

testRenumberingOutput = [
 Make "1" "division" "eg.Division" [],
 Call "2" "division" "setNumerator" [ "10" ],
 Import "3" "division",
 Call "4" "division" "setDenominator" [ "2" ],
 CallAndAssign "5" "division" "toto" "setDenominator" [ "2" ],
 Call "6" "division" "quotient" []
 ]


testCanRenumberInstructions = "renumbering instructions" ~:
                              TestList [
                       "renumbering One Make element" ~:
                       renumber [ Make "0" "division" "eg.Division" [] ] ~?= [ Make "1" "division" "eg.Division" [] ]
                       ,"renumbering one Call element" ~:
                       renumber [ Call "" "division" "eg.Division" [] ] ~?= [ Call "1" "division" "eg.Division" [] ]
                       ,"renumbering one CallAndAssign element" ~:
                       renumber [ CallAndAssign "" "division" "toto" "eg.Division" [] ] ~?= [ CallAndAssign "1" "division" "toto" "eg.Division" [] ]
                       ,"renumbering one Import element" ~:
                       renumber [ Import "5" "division" ] ~?= [ Import "1" "division" ]
                       ,"renumbering list" ~:
                       renumber testRenumberingInput ~?= testRenumberingOutput
                              ]

                           
-- | Runs only if test is run in current directory
slimClientSendAndReceiveMessages =
    TestList [
       "slim client w/ match" ~: (runSlim (doSlim testDivisionInput) defaultSlim >>= 
                                             (assertEqual "correct i/o and match with slim"  [
                                                               (Make "1" "division" "eg.Division" [], A $ S "OK"),
                                                               (Call "2" "division" "setNumerator" [ "10" ], A $ S "/__VOID__/"),
                                                               (Call "3" "division" "setDenominator" [ "2" ], A $ S "/__VOID__/"),
                                                               (Call "4" "division" "quotient" [], A $ S "5.0")
                                                              ])),
       "sequence of slim sendings" ~: (runSlim (doSequenceOfCalls testDivisionInput) defaultSlim) >>= 
                                             (assertEqual "correct i/o and match with slim"  [
                                                               (Make "1" "division" "eg.Division" [], A $ S "OK"),
                                                               (Call "2" "division" "setNumerator" [ "10" ], A $ S "/__VOID__/"),
                                                               (Call "3" "division" "setDenominator" [ "2" ], A $ S "/__VOID__/"),
                                                               (Call "4" "division" "quotient" [], A $ S "5.0")
                                                              ])
  ]
      where
        doSequenceOfCalls calls = do startSlim
                                     answers <- (doSendSlim calls)
                                     endSlim
                                     return answers
        

matchInstructionsToAnswers =
   "match instructions" ~: 
   TestList [
        matchQandA [Make "1" "division" "eg.Division" []] Nothing
                       ~?= [(Make "1" "division" "eg.Division" [], KO "No answer from client")],
        matchQandA [Make "1" "division" "eg.Division" []] (Just (KO "error"))
                       ~?= [(Make "1" "division" "eg.Division" [], KO "error")],
        matchQandA [Make "1" "division" "eg.Division" []] (Just $ A $ L [L [S "1",L [ S "1", S "2"]]])
                       ~?= [(Make "1" "division" "eg.Division" [], A $ L [ S "1", S "2"])],
        matchQandA [Make "1" "division" "eg.Division" []] (Just $ A $ L [L [S "2",S "OK"]])
                       ~?= [(Make "1" "division" "eg.Division" [], KO "No answer from client")],                           
        matchQandA [ Call "2" "division" "setNumerator" [ "10" ]] (Just $ A $ L [L [S "2",S "/__VOID__/"]])
                       ~?= [(Call "2" "division" "setNumerator" [ "10" ], A $ S "/__VOID__/")],                           
      matchQandA testDivisionInput (Just testDivisionOutput)  ~?= 
      [
       (Make "1" "division" "eg.Division" [], A $ S "OK"),
       (Call "2" "division" "setNumerator" [ "10" ], A $ S "/__VOID__/"),
       (Call "3" "division" "setDenominator" [ "2" ], A $ S "/__VOID__/"),
       (Call "4" "division" "quotient" [], A $ S "5.0")
      ],                           
      matchQandA (rotate 1 testDivisionInput) (Just testDivisionOutput)  ~?= 
      [
       (Call "2" "division" "setNumerator" [ "10" ], A $ S "/__VOID__/"),
       (Call "3" "division" "setDenominator" [ "2" ], A $ S "/__VOID__/"),
       (Call "4" "division" "quotient" [], A $ S "5.0"),
       (Make "1" "division" "eg.Division" [], A $ S "OK")
      ]
   ]
    where
      rotate :: Int -> [ a ] -> [ a ]
      rotate n xs = let (h,t) = splitAt n xs
                    in t ++ h

