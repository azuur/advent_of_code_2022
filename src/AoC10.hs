module AoC10 where

import Data.Functor
import Data.List.Split(splitOn, chunksOf)

dummyInput = 
  [ "addx 15"
  , "addx -11"
  , "addx 6"
  , "addx -3"
  , "addx 5"
  , "addx -1"
  , "addx -8"
  , "addx 13"
  , "addx 4"
  , "noop"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx -35"
  , "addx 1"
  , "addx 24"
  , "addx -19"
  , "addx 1"
  , "addx 16"
  , "addx -11"
  , "noop"
  , "noop"
  , "addx 21"
  , "addx -15"
  , "noop"
  , "noop"
  , "addx -3"
  , "addx 9"
  , "addx 1"
  , "addx -3"
  , "addx 8"
  , "addx 1"
  , "addx 5"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx -36"
  , "noop"
  , "addx 1"
  , "addx 7"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "addx 6"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx 7"
  , "addx 1"
  , "noop"
  , "addx -13"
  , "addx 13"
  , "addx 7"
  , "noop"
  , "addx 1"
  , "addx -33"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "noop"
  , "noop"
  , "noop"
  , "addx 8"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx 17"
  , "addx -9"
  , "addx 1"
  , "addx 1"
  , "addx -3"
  , "addx 11"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx -13"
  , "addx -19"
  , "addx 1"
  , "addx 3"
  , "addx 26"
  , "addx -30"
  , "addx 12"
  , "addx -1"
  , "addx 3"
  , "addx 1"
  , "noop"
  , "noop"
  , "noop"
  , "addx -9"
  , "addx 18"
  , "addx 1"
  , "addx 2"
  , "noop"
  , "noop"
  , "addx 9"
  , "noop"
  , "noop"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx -37"
  , "addx 1"
  , "addx 3"
  , "noop"
  , "addx 15"
  , "addx -21"
  , "addx 22"
  , "addx -6"
  , "addx 1"
  , "noop"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx -10"
  , "noop"
  , "noop"
  , "addx 20"
  , "addx 1"
  , "addx 2"
  , "addx 2"
  , "addx -6"
  , "addx -11"
  , "noop"
  , "noop"
  , "noop"
  ]

readInput :: IO [Instruction]
readInput = readFile "input/10" <&> lines <&> map readInstruction

data Instruction = Noop | Addx Int deriving (Show)

readInstruction :: String -> Instruction
readInstruction "noop" = Noop
readInstruction str = Addx ((read :: String -> Int) . last .   splitOn " " $ str)

cpuCycle :: [Int] -> Instruction -> [Int]
cpuCycle [] _ = []
cpuCycle (x : xs) Noop = x : (x : xs)
cpuCycle (x : xs) (Addx y) = (x + y) : x : (x : xs)

execution :: [Instruction] -> [Int]
execution = reverse . foldl cpuCycle [1]

solve1 :: [Instruction] -> Int
solve1 instructions = answer where
  run = execution instructions
  extractStrength x = x * (run !! (x - 1))
  keyCycles = [20, 60, 100, 140, 180, 220]
  strengths = map extractStrength keyCycles
  answer = sum strengths

drawLine :: [(Int, Int)] -> String
drawLine [] = []
drawLine ((nCycle, x) : cont)
  | nCycle <= x + 1 && nCycle >= x - 1      = '#' : drawLine cont
  | otherwise      = '.' : drawLine cont

solve2 instructions = unlines ls where
  ls = map (drawLine . zip [0..39]) ls'
  ls' = chunksOf 40 (execution instructions)

main = do
  print . solve1 . map readInstruction $ dummyInput
  readInput >>= print . solve1
  putStrLn . solve2 . map readInstruction $ dummyInput
  readInput >>= putStrLn . solve2
