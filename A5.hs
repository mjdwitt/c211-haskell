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

-- | A version using foldl.
isPrefix' :: (Eq a) => [a] -> [a] -> Bool

isPrefix' xs ys = let f :: (Eq a) => ([a],Bool) -> a -> ([a],Bool)
                      f (ys,    False) x
                                      = (ys,False)
                      f ([],    _    ) x
                                      = ([],False)
                      f ((y:ys),end) x
                          | x == y    = (ys,True)
                          | otherwise = (ys,False)
                  in snd $ foldl' f (ys,True) xs



-- Problem 10
-- | Appends two lists, except if the last n items of the first are
-- the same values as their corresponding first n items of the second,
-- the "overlap" only appears once in the produced list. For example,
--
--     fuse [1,2,3,4,5] [4,5,6,7,8]
--     == [1,2,3,4,5,6,7,8]
--
fuse :: Eq a => [a] -> [a] -> [a]

fuse xs@(x:rest) ys | xs `isPrefix'` ys = ys
                    | otherwise         = x : fuse rest ys
fuse _           ys                     = ys



-- Problem 11
-- | Filters out any elements from a list which are equivalent to one
-- preceeding them.
unique :: Eq a => [a] -> [a]

unique xs = deduplicate xs []
            where deduplicate :: Eq a => [a] -> [a] -> [a]
                  deduplicate (x:xs) out | x `elem` out = deduplicate xs out
                                         | otherwise    = deduplicate xs $ out ++ [x]
                  deduplicate   _    out                = out

-- | A folding version of the above.
unique' :: Eq a => [a] -> [a]

unique' = let f :: Eq a => [a] -> a -> [a]
              f left x | x `elem` left = left
                       | otherwise     = left ++ [x]
          in foldl' f []
