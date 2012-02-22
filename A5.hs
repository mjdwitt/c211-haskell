-- Problem 2
-- | A generalization of the actual problem from assignment 2 which takes a
-- predicate and makes any passing value appear twice in a row each time it
-- appears in a given list
doubler :: (a -> Bool) -> [a] -> [a]

doubler p = foldr double []
            where double x end | p x       = x : (x : end)
                               | otherwise = x : end
