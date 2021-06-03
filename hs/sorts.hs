
module Main where

import qualified Data.List       as List
import           Test.Hspec
import           Test.QuickCheck

type Cmp a = a -> a -> Ordering


insertionSort :: Ord a => Cmp a -> [a] -> [a]
insertionSort comparing = foldr (insertEl comparing) []
    where insertEl :: Ord a => Cmp a -> a -> [a] -> [a]
          insertEl comparing x [] = [x]
          insertEl comparing x (y:xs) =
              case comparing x y of
                LT -> x:y:xs
                _  -> y:insertEl comparing x xs


mergeSort :: Ord a => Cmp a -> [a] -> [a]
mergeSort _ []         = []
mergeSort _ [x]        = [x]
mergeSort comparing xs =
    merge_ comparing (mergeSort comparing left_) (mergeSort comparing right_)
    where
        nelements = length xs
        left_ = take (nelements `div` 2) xs
        right_ = drop (nelements `div` 2) xs

        merge_ :: Cmp a -> [a] -> [a] -> [a]
        merge_ _ [] rhs = rhs
        merge_ _ lhs [] = lhs
        merge_ cmp (l:lhs) (r:rhs) = case cmp l r of
          LT -> l:merge_ cmp lhs (r:rhs)
          _  -> r:merge_ cmp (l:lhs) rhs


main :: IO ()
main = hspec $ do

    it "test insertion sort" $
        property testInsertionSort

    it "test merge sort" $
        property testMergeSort

    where
        testInsertionSort :: [Int] -> Bool
        testInsertionSort xs = List.sort xs == insertionSort compare xs

        testMergeSort :: [Int] -> Bool
        testMergeSort xs = List.sort xs == mergeSort compare xs
