-- Problem 5
-- | Produces a list where each element equal to a is swapped for the
-- value b and vice versa.
swapElements :: Eq a => a -> a -> [a] -> [a]

swapElements a b = swap
                   where swap (x:xs) | x == a    = b : swap xs
                                     | x == b    = a : swap xs
                                     | otherwise = x : swap xs
                         swap   _                =   []

-- | A folding version.
swapElements' :: Eq a => a -> a -> [a] -> [a]

swapElements' a b = let f x xs | x == a    = b : xs
                               | x == b    = a : xs
                               | otherwise = x : xs
                    in foldr f []



-- Problem 6
-- A simple calculator.

-- | A type representing the buttons on the calculator.
data Button = N Int
            | O (Int -> Int -> Int)
            | Z

-- | Takes a list of input button presses and performs the represnted
-- calculation.
simpleCalc :: [Button] -> Int

simpleCalc input = calc Z (N 0) Z input
                   where calc   l   (N r)   o   (  (N d) : input) = calc l (N (r*10+d)) o input
                         calc   _     r     Z   (o@(O _) : input) = calc r (N 0) o input
                         calc (N l) (N r) (O o) (p@(O _) : input) = calc (N (l `o` r)) (N 0) p input
                         calc   _     _     _   (   Z    : _    ) = error "how could you possibly have input not pressing a button?"
                         calc (N l) (N r) (O o)          _        = l `o` r
                         calc   Z   (N r)   _            _        = r



-- Problem 8
-- simulating a browser's history stack

-- | A type representing the possible navigation options in our browser.
data BrowserInstruction = Page String
                        | Forward
                        | Back
                          deriving (Show)

-- | Takes a list of browser instructions and an initial page and returns
-- the result of executing the instructions in the list. Attempts to go
-- back beyond an empty back history or forwards beyond an empty forward
-- history are ignored.
browse :: BrowserInstruction -> [BrowserInstruction] -> BrowserInstruction

browse p@(Page _) instructions = session p instructions [] []
  where session p (p'@(Page _):ins)    bck      fwd    = session p' ins (p:bck)  []
        session p      ((Back):ins)  (p':bck)   fwd    = session p' ins  bck    (p:fwd)
        session p      ((Back):ins)    []       fwd    = session p  ins  []      fwd
        session p   ((Forward):ins)    bck    (p':fwd) = session p' ins (p:bck)  fwd
        session p   ((Forward):ins)    bck      []     = session p  ins  bck     []
        session p            []        _        _      = p

browse  _         _            = error "browse: must start at a page"



-- Problem 9
-- DNA crossover

-- | A type representing DNA nucleotides.
data Nucleotide = A
                | C
                | G
                | T
                  deriving (Show, Eq)

-- | Simulates chromosome crossover when given two DNA samples (lists of
-- Nucleotides) and an index at which to begin the crossover.
crossover :: [Nucleotide] -> [Nucleotide] -> Integer -> [Nucleotide]

crossover (x:xs) py@(y:ys) cross | cross == 0 = py
                                 | otherwise  = x : crossover xs ys (cross - 1)



-- Problem 10
-- context-aware list mutation

-- | Takes a function of three Nucleotides and applies it to each
-- overlapping set of three adjacent Nucleotides in a list. The resulting
-- Nucleotide is then mapped to the same position as the center in the
-- returned list.
mutate :: (Nucleotide -> Nucleotide -> Nucleotide -> Nucleotide)
       -> [Nucleotide]
       -> [Nucleotide]

mutate m xs@(x:_) = x : mutator xs
  where mutator (x:(xs@(y:(z:_)))) = m x y z : mutator xs
        mutator xs                 = tail xs

-- A test mutation function.
mut x y z = T



-- Problem 11

-- | A generalization of the above crossover which takes a list of
-- crossover points.
crossoverAll :: [Nucleotide] -> [Nucleotide] -> [Integer] -> [Nucleotide]

crossoverAll 
