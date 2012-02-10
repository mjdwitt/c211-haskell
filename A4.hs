-- Michael DeWitt

module A4 where

import A3 (nextCollatz)



-- Problem 2

-- | 'couple' constructs a two-element list with the given two items
couple :: a -> a -> [a]

couple x y = x:(y:[])

-- | 'neighbors' takes an integer and uses couple to build a list of
-- the integers to either side
neighbors :: Integral a => a -> [a]

neighbors x = couple (x-1) (x+1)



-- Problem 3

-- | 'sumSquares' takes a natural number n and returns the sum of the
-- squares of the first n natural numbers
--
-- > sumSquares n == n^2 + (n-1)^2 + ... + 2^2 + 1^2
sumSquares :: Integral a => a -> a

sumSquares 1 = 1
sumSquares n = n^2 + sumSquares (n-1)



-- Problem 4

-- | 'Tribble's are a data type of one possible value
data Tribble = Tribble
               deriving(Show)

-- | 'tribbles' creates a list of the given number of Tribbles
tribbles :: Integral a => a -> [Tribble]

tribbles 0 = []
tribbles n = Tribble : tribbles (n-1)

-- | 'troubleWithTribbles' doubles the given list of Tribbles
troubleWithTribbles :: [Tribble] -> [Tribble]

troubleWithTribbles ts = doubleTribbles ts ts
    where doubleTribbles end (t:ts) = t : doubleTribbles end ts
          doubleTribbles end _      = end

-- This would be better using (++)

troubleWithTribbles' ts = ts ++ ts



-- Problem 7

-- | 'listBump' takes a list and returns a list with each number incremented
listBump :: Num a => [a] -> [a]

listBump = foldr bump []
    where bump x end = x+1 : end



-- Problem 8

-- | 'collatzSteps' takes a first term of a Collatz sequence and returns
-- the number of terms between it and the first term equal to one
collatzSteps :: Integral a => a -> a

collatzSteps 1 = 0
collatzSteps n = 1 + collatzSteps (nextCollatz n)

-- | 'collatzSequence' takes a first term of a Collatz sequence and returns
-- the list containing all the terms from it to the first term equal to
-- one, inclusive.
collatzSequence :: Integral a => a -> [a]

collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (nextCollatz n)



-- Problem 9

findMin (x:[]) = x
findMin xs     = findMin (crunchFront xs)
  where crunchFront (x:y:xs) = min x y : xs



-- Problem 11

data SpaceTalker = Apollo
                 | Houston
                   deriving (Show,Eq)

-- | 'spaceTalk' takes a list of items and returns a list such that each
-- item is a pair containing the original list item and either Apollo or
-- Houston. Which SpaceTalker the item gets paired with alternated for
-- each list item.
spaceTalk :: [a] -> [(SpaceTalker,a)]

spaceTalk = alternator Apollo
    where alternator talker (x:xs) = (talker,x) : alternator talker' xs
              where talker' = if talker == Apollo
                              then Houston
                              else Apollo
          alternator _      _      = []
