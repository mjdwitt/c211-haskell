import Data.List



-- Problem 2
-- | A generalization of the actual problem from assignment 2 which takes a
-- predicate and makes any passing value appear twice in a row each time it
-- appears in a given list
doubler :: (a -> Bool) -> [a] -> [a]

doubler p = foldr double []
            where double x end | p x       = x : (x : end)
                               | otherwise = x : end



-- Problem 3
-- | This generalizes the original penny-pincher into a function which
-- returns a list with all elements passing a given procedure removed
-- from the given list. The opposite of filter.
remove :: (a -> Bool) -> [a] -> [a]

remove p = filter (\x -> not . p $ x)



-- Problem 6-B
-- | Returns each element in both lists a maximum of once. The resulting
-- list is equivalent to removing all of the elements from the first given
-- that appear in the second and appending the second onto the end of the
-- first.
intersection :: (Eq a) => [a] -> [a] -> [a]

intersection xs ys = let f x z | x `elem` z = z
                               | otherwise  = x : z
                     in foldr f ys xs



-- Problem 9
-- | True if the first list is a prefix of the second.
isPrefix :: (Eq a) => [a] -> [a] -> Bool

isPrefix (x:xs) (y:ys) | x == y    = isPrefix xs ys
                       | otherwise = False
isPrefix (x:xs)   []               = False
isPrefix   []     _                = True

-- | A refactored version using foldr.
isPrefix' :: (Eq a) => [a] -> [a] -> Bool

