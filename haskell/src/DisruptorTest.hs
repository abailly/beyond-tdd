import Test.HUnit
import IO (stderr)
import System.Exit
import Disruptor
import Data.Bits (shiftL)

tests = TestList [
  testRingBuffer,
  testBitOperations
  ]

testBitOperations = 
  "computing power of 2 bound should" ~: TestList [
    powerOf2Bound 0 ~?= 0,
    powerOf2Bound 1 ~?= 2,
    powerOf2Bound 2 ~?= 2,
    powerOf2Bound 3 ~?= 4,
    powerOf2Bound 4 ~?= 4,
    powerOf2Bound 5 ~?= 8,
    powerOf2Bound 6 ~?= 8,
    powerOf2Bound 8 ~?= 8,
    powerOf2Bound 15 ~?= 16,
    powerOf2Bound 17 ~?= 32
  ]


testRingBuffer =
  "a ring buffer should" ~: TestList [
    "be created with 2^n slots" ~: TestList [
       size (makeRingBuffer 3) ~?= 4,
       size (makeRingBuffer 5) ~?= 8
       ]
    ]

main = do counts <- runTest tests
          case (errors counts + failures counts) of
            0 -> exitWith ExitSuccess
            n -> exitWith (ExitFailure n)

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts

