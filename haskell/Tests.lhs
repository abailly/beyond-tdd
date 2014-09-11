> module Main
> where

> import Data.List
> import Test.QuickCheck
> import Control.Monad
>

We have a small program, in which a function sublist gives all the sublists 
possible from a list. Here it is : 

> sublists [] = [[]]
> sublists (x:xs) = s ++ map (x:) s where s = sublists xs

Is this program correct ? Let's test it!

What properties could we test so that we have confidence that the program works ?

First we need to limit ourselves to a small number of cases 

> small :: [Int] -> Bool
> small xs = (length xs) < 10
>
> smallLists :: Int -> Gen [ Int ]
> smallLists n = choose (1,n) >>= vector
>

The number of possible sublists extracted from a list of size N is 2^N

> twoPowerNSubLists :: Property
> twoPowerNSubLists  = forAll (smallLists 5) $ \xs ->  
>                      length (sublists xs) == 2 ^ (length xs)

If the list contains no duplicate, then the list of sublists contains no duplicate

> noDups :: (Eq a) => [a] -> Bool
> noDups xs = (nub xs) == xs

> noDuplicates :: [Int] -> Property
> noDuplicates xs = forAll (smallLists 5) $ \xs ->
>                   noDups xs ==> noDups (sublists xs)

Each item in the result list is a sublist form the initial list

> isASubListFrom :: (Eq a) => [a] -> [a] -> Bool
> []     `isASubListFrom` _               = True
> (x:xs) `isASubListFrom` (y:ys) | x == y = xs `isASubListFrom` ys
> (x:xs) `isASubListFrom` (y:ys) | x /= y = (x:xs) `isASubListFrom` ys
> (x:xs) `isASubListFrom` []              = False


> allSubLists :: [Int] -> Property
> allSubLists xs = forAll (smallLists 5)$ \xs -> (collect $ length xs) $ and (map (`isASubListFrom` xs) (sublists xs))

Why generating sublists ? Because we want to generate all sublists of a list of orders !

> data Order = Order { start :: Int,
>                      end   :: Int,
>                      price :: Double }
>              deriving (Show, Eq)
>
> (<<<) :: Order -> Order -> Bool
> _ <<< _ = False
>
> instance Arbitrary Order where
>     arbitrary = do s <- choose (0,23)
>                    d <- choose (1,10)
>                    p <- choose (10,100)
>                    return $ Order s (s + d) p
>                          
> propCompatible o1 o2 = (start o2) >= (end o1) ==> o1 <<< o2
>                       

> main = do quickCheck twoPowerNSubLists
>	    quickCheck noDuplicates
>	    quickCheck allSubLists
>           quickCheck propCompatible








