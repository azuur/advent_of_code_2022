module AoC03 where

import Data.Functor
import qualified Data.Set as Set
import Data.List.Split(chunksOf) 
import Data.Maybe(fromJust)

dummyInput = 
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

readInput :: IO [String]
readInput = readFile "input/03"
 <&> lines

splitLine :: String -> (String, String)
splitLine str = (head sub, last sub) where 
    sub = chunksOf (length str `div` 2) str

findCommon :: String -> String -> Maybe Char
findCommon [] ys = Nothing
findCommon (x : xs) ys = if x `elem` ys then Just x else findCommon xs ys

idxMatch :: Char -> String -> Int
idxMatch x [] = 0
idxMatch x (y : ys) = 1 + if x == y then 0 else idxMatch x ys

priority :: Char -> Int
priority x = idxMatch x (['a'..'z'] ++ ['A'..'Z'])

solve1 :: [String] -> Int
solve1 = sum . map (priority . fromJust . uncurry findCommon . splitLine)

intersection' :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
intersection' q s = if null q then s else Set.intersection q s

findBadge :: [String] -> Char
findBadge = head . Set.toList . foldl intersection' Set.empty . map Set.fromList

solve2 :: [String] -> Int
solve2 = sum . map (priority . findBadge) . chunksOf 3


main :: IO ()
main = do
--  readInput >>= print
  print (solve1 dummyInput)
  readInput >>= print . solve1
  print (solve2 dummyInput)
  readInput >>= print . solve2  
