module Disruptor where
import Data.Array.IArray
import Data.Bits

data RingBuffer = RingBuffer (Array Int Int)
                  deriving (Eq, Show)


powerOf2Bound :: Int -> Int
powerOf2Bound i | i > 1 = bound 31 i 
powerOf2Bound 1 = 2
powerOf2Bound 0 = 0

bound n i | (i > shiftL 1 (n-1)) && not (testBit i n)  = shiftL 1 n
          | bit (n-1) `xor` i == 0                     = i
          | otherwise                                  = bound (n-1) i

makeRingBuffer :: Int -> RingBuffer
makeRingBuffer size = RingBuffer $ array (0,powerOf2Bound size -1) [ (i,0) | i <- [0..powerOf2Bound size-1]]

size :: RingBuffer -> Int
size (RingBuffer a) = (rangeSize.bounds) a