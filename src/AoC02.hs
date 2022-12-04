module AoC02 where

import Data.Functor
import Data.Bifunctor
import Data.Function
import Data.List.Split(splitWhen) 

readInput :: IO [(Char, Char)]
readInput = readFile "input/02"
 <&> lines
 <&> fmap (splitWhen (==' '))
 <&> fmap (map head)
 <&> fmap (\l -> (head l, last l))

data Plays = Rock | Paper | Scissors

oppoMap :: Char -> Plays
oppoMap 'A' = Rock
oppoMap 'B' = Paper
oppoMap 'C' = Scissors

myMap :: Char -> Plays
myMap 'X' = Rock
myMap 'Y' = Paper
myMap 'Z' = Scissors

playValue :: Plays -> Int
playValue Rock = 1
playValue Paper = 2
playValue Scissors = 3

game :: Plays -> Plays -> Int
game Rock Rock = 3
game Rock Paper = 6
game Rock Scissors = 0
game Paper Rock = 0
game Paper Paper = 3
game Paper Scissors = 6
game Scissors Rock = 6
game Scissors Paper = 0
game Scissors Scissors = 3

totalScore :: Plays -> Plays -> Int
totalScore oppo mine = game oppo mine + playValue mine

solve1 :: [(Char, Char)] -> Int
solve1 = sum . map (uncurry totalScore . bimap oppoMap myMap)

selectPlay :: Plays -> Char -> Plays
selectPlay Rock 'X' = Scissors
selectPlay Rock 'Y' = Rock
selectPlay Rock 'Z' = Paper
selectPlay Paper 'X' = Rock
selectPlay Paper 'Y' = Paper
selectPlay Paper 'Z' = Scissors
selectPlay Scissors 'X' = Paper
selectPlay Scissors 'Y' = Scissors
selectPlay Scissors 'Z' = Rock

solve2 :: [(Char, Char)] -> Int
solve2 strategy = sum $ map (uncurry totalScore) plays where
  plays = zip oppoPlays myPlays
  oppoPlays = map (oppoMap . fst) strategy
  myPlays = zipWith selectPlay oppoPlays (map snd strategy)


dummyInput = [('A', 'Y'), ('B', 'X'), ('C', 'Z')]

main :: IO ()
main = do
  readInput >>= print
  print (solve1 dummyInput)
  readInput >>= print . solve1
  print (solve2 dummyInput)
  readInput >>= print . solve2  

