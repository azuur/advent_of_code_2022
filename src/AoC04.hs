module AoC04 where

import Data.Functor
import Data.List.Split(splitWhen)
import Data.Char(digitToInt)
import qualified Data.Set as Set
-- import Data.List.Split(chunksOf) 
-- import Data.Maybe(fromJust)


lineToTuples :: String -> ((Int, Int), (Int, Int))
lineToTuples s = ((nums !! 0, nums !! 1), (nums !! 2, nums !! 3)) where
  nums = map (read :: String -> Int) (splitWhen (`elem` ",-") s)

dummyInput = map lineToTuples
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]


readInput :: IO [((Int, Int), (Int, Int))]
readInput = readFile "input/04"
 <&> lines
 <&> fmap lineToTuples


contains :: ((Int, Int), (Int, Int)) -> Int
contains ((a, b), (c, d)) = fromEnum $ (a <= c && b >= d) || (a >= c && b <= d)

overlaps :: ((Int, Int), (Int, Int)) -> Int
overlaps ((a, b), (c, d)) = fromEnum $ (b >= c && a <= d) || (d >= a && c <= b)
-- overlaps' ((a, b), (c, d)) = fromEnum . not . Set.null $ Set.intersection (Set.fromList [a..b]) (Set.fromList [c..d])

solve1 :: [((Int, Int), (Int, Int))] -> Int
solve1 = sum . map contains

solve2 :: [((Int, Int), (Int, Int))] -> Int
solve2 = sum . map overlaps
--solve2 = map (\(x, y) -> if overlaps (x, y) /= overlaps' (x, y) then Just (x, y) else Nothing)

main :: IO ()
main = do
  print (solve1 dummyInput)
  readInput >>= print . solve1
  print (solve2 dummyInput)
  readInput >>= print . solve2  
