-- Michael DeWitt



-- Problem 3:

-- | 'larger` takes two numbers and returns the larger of the two

larger x y | x > y     = x
           | otherwise = y

-- | 'largest' takes three numbers and uses 'larger' to find the largest of
-- three numbers

largest x y z = larger x $ larger y z



-- Problem 12

-- | 'nextCollatz' takes a number defining the beginning of a Collatz
-- sequence and returns the next term

nextCollatz x | even x    = x `div` 2
              | otherwise = 3*x + 1

