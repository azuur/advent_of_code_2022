module AoC06 where

import Data.Functor

readInput :: IO String
readInput = readFile "input/06"

dummyInputs = 
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  , "bvwbjplbgvbhsrlpgdmjqwftvncz"    
  , "nppdvjthqldpwncqszvftbrmjlhg"
  , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

noRepeats :: String -> Bool
noRepeats [] = True
noRepeats [_] = True
noRepeats (x: xs) = notElem x xs && noRepeats xs

noRepeatsConditions :: Int -> String -> [Bool]
noRepeatsConditions n xs | length xs < n = []
                         | otherwise = noRepeats (take n xs) : noRepeatsConditions n (tail xs)

packetConditions :: String -> [Bool]
packetConditions = noRepeatsConditions 4

messageConditions :: String -> [Bool]
messageConditions = noRepeatsConditions 14

argMaxes :: Ord a => [a] -> [Int]
argMaxes [] = []
argMaxes xs = filter ((== maximum xs) . (!!) xs) [0..(length xs - 1)]

solve1 :: String -> Int
solve1 =  (+4) . head . argMaxes . packetConditions

solve2 :: String -> Int
solve2 =  (+14) . head . argMaxes . messageConditions


main = do
  print $ map solve1 dummyInputs
  readInput >>= print . solve1
  print $ map solve2 dummyInputs
  readInput >>= print . solve2