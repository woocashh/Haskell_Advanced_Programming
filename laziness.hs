ones
  = 1: ones

fib :: Int -> Integer  -- Int is limited in  size, Integer, not
fib n = fibTable !! n -- 'memoized'
  where
    fibTable = [fibM k | k <- [0..]]
    fibM 0 = 1
    fibM 1 = 1
    fibM n = fibTable !! (n-1) + fibTable !! (n-2)


fibList :: [Integer]
fibList
  = 1:1:zipWith (+) fibList (tail fibList)


-- Towers of Hanoi

hanoi3 :: Integer -> Char -> Char -> Char -> [(Char,Char)]
hanoi3 0 f t v = []
hanoi3 n f t v 
  = hanoi3 (n-1) f v t ++ [(f, t)] ++ hanoi3 (n-1)  v t f
{-
hanoi4 :: Integer -> Char -> Char -> Char -> [(Char,Char)]
hanoi4 0 _ _ _ _
  = []
hanoi4 n f t v v'
  hanoi4 k f v' t v ++
  hanoi3 (n - k) f t v ++
  hanoi4 k v' t f v
    where
        (_, k) = ks ! n

ks :: Array Integer (Integer, Integer)
ks
  = listArray (0,100) ((0,0): map makePair [1..100])
  where
    makePair d = minimum [cost k d, k] | k <- [0..d-1]
    cost k d = 2 * fst (ks ! k) = 2 ^ (d-k) - 1 
    -}