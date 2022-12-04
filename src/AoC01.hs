module AoC01 where

import Data.Functor
import Data.Function
import Data.List(sortBy)
import Data.List.Split(splitOn, splitWhen) 

readInput :: IO [[Int]]
readInput = readFile "input/01"
 <&> lines
 <&> splitWhen (=="")
 <&> fmap (map read)

totalCalories :: (Ord a) => (Num a) => [[a]] -> [a]
totalCalories = map sum

solve1 :: (Ord a) => (Num a) => [[a]] -> a
solve1 = maximum . totalCalories


solve2 :: (Ord a) => (Num a) => [[a]] -> a
solve2 xs = sum (take 3 (sortBy (flip compare) (totalCalories xs)))

dummyInput :: [[Int]]
dummyInput = [[2,1,3], [4,4], [1,2,1]]

main :: IO ()
main = do
  readInput >>= print . solve1
  readInput >>= print . solve2
