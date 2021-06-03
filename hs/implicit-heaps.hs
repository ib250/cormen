
module Main where


newtype Heap a =
  Heap { unHeap :: [a] } deriving Eq

instance Show a => Show (Heap a) where
  show = unlines . fmap show . levels

emptyHeap :: Heap a
emptyHeap = Heap []


pushHeap :: Ord a => a -> Heap a -> Heap a
pushHeap x (Heap []) = Heap [x]
pushHeap x (Heap (y:xs))
  | x > y     = Heap (x:y:xs)
  | otherwise = Heap (y:rest)
  where rest = unHeap $ pushHeap x (Heap xs)


popHeap :: Ord a => Heap a -> (a, Heap a)
popHeap = popHeap


mkHeap :: Ord a => [a] -> Heap a
mkHeap = foldr pushHeap emptyHeap


levels :: Heap a -> [[a]]
levels (Heap xs) =
  [take n (drop start xs) | (start, n) <- zip levels_ (tail levels_)]
  where
    levels_ = 0:takeWhile (<length xs) [2^i | i <- [0..]]


main :: IO ()
main = print $ mkHeap [1..10]
