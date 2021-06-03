{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Test.Hspec


data MaxHeapOrder
data MinHeapOrder

class HeapOrder a where
    getHeapOrder :: Ordering


instance HeapOrder MaxHeapOrder where
  getHeapOrder = GT


instance HeapOrder MinHeapOrder where
  getHeapOrder = LT


data Heap ord a = Empty
    | Node { left  :: Heap ord a
           , value :: a
           , right :: Heap ord a
           } deriving (Eq, Show)


type MaxHeap a = Heap MaxHeapOrder a

type MinHeap a = Heap MinHeapOrder a


instance HeapOrder ord => Semigroup (Heap ord a) where
  (<>) = undefined

instance HeapOrder ord => Monoid (Heap ord a) where
  mempty = Empty


singletonHeap :: HeapOrder ord => a -> Heap ord a
singletonHeap x = Node Empty x Empty


mkHeap :: (HeapOrder ord, Ord a) => [a] -> Heap ord a
mkHeap = foldMap singletonHeap


mkMaxHeap :: Ord a => [a] -> MaxHeap a
mkMaxHeap = mkHeap

mkMinHeap :: Ord a => [a] -> MinHeap a
mkMinHeap = mkHeap


sampleHeap :: HeapOrder ord => Heap ord Int
sampleHeap = mkHeap [1,4,2,3,5,2,5,6,2,4,5,9]


unHeap :: HeapOrder ord => Heap ord a -> [a]
unHeap Empty            = []
unHeap (Node lhs x rhs) = x:unHeap' (lhs, rhs)
  where unHeap' (Empty, Empty)                 = []
        unHeap' (Node l y r, Empty)            = y:unHeap' (l, r)
        unHeap' (Empty, Node l y r)            = y:unHeap' (l, r)
        unHeap' (Node l1 y1 r1, Node l2 y2 r2) =
          let rest = unHeap' (l1, r1) ++ unHeap' (l2, r2) in y1:y2:rest



test_unHeap :: Spec
test_unHeap = describe "test unheap runs in bfs order" $ do

  let xs = Node (singletonHeap 1) 3 (singletonHeap 2) :: MaxHeap Int
  let xs' = [3, 1, 2]
  it "shallow tree maxHeap" $ (unHeap xs) `shouldBe` xs'

  let ys = Node xs 10 xs :: MaxHeap Int
  let ys' = [10, 3, 3, 1, 2, 1, 2]
  it "deeper balanced tree maxHeap" $ (unHeap ys) `shouldBe` ys'

  let zs = Node xs 10 Empty :: MaxHeap Int
  let zs' = [10, 3, 1, 2]
  it "unbalanced tree maxHeap - left biased" $ (unHeap zs) `shouldBe` zs'

  let zs = Node Empty 10 xs :: MaxHeap Int
  let zs' = [10, 3, 1, 2]
  it "unbalanced tree maxHeap - right biased" $ (unHeap zs) `shouldBe` zs'

  let xs = Node (singletonHeap 1) 3 (singletonHeap 2) :: MinHeap Int
  let xs' = [3, 1, 2]
  it "shallow tree minHeap" $ (unHeap xs) `shouldBe` xs'

  let ys = Node xs 10 xs :: MinHeap Int
  let ys' = [10, 3, 3, 1, 2, 1, 2]
  it "deeper balanced tree minHeap" $ (unHeap ys) `shouldBe` ys'

  let zs = Node xs 10 Empty :: MinHeap Int
  let zs' = [10, 3, 1, 2]
  it "unbalanced tree minHeap - left biased"  $ (unHeap zs) `shouldBe` zs'

  let zs = Node Empty 10 xs :: MinHeap Int
  let zs' = [10, 3, 1, 2]
  it "unbalanced tree minHeap - right biased"  $ (unHeap zs) `shouldBe` zs'



main :: IO ()
main = hspec $ do
  test_unHeap
