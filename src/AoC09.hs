module AoC09 where

import Data.Functor
import Data.List.Split(splitOn)
import Data.Bifunctor(first)
import Data.Char(digitToInt)

dummyInput = 
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

dummyInput2 =
  [ "R 5"
  , "U 8"
  , "L 8"
  , "D 3"
  , "R 17"
  , "D 10"
  , "L 25"
  , "U 20"
  ]

readInput :: IO [String]
readInput = readFile "input/09" <&> lines <&> concatMap interpretLine

interpretLine :: String -> [String]
interpretLine line = take n (repeat m) where
  instruction = splitOn " " line
  n = (read :: String -> Int) . last $ instruction
  m = head instruction


unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = [x | x `notElem` xs] ++ unique xs

moveH :: (Int, Int) -> String -> (Int, Int)
moveH (xH, yH) "R" = (xH + 1, yH + 0)
moveH (xH, yH) "L" = (xH - 1, yH + 0)
moveH (xH, yH) "U" = (xH + 0, yH + 1)
moveH (xH, yH) "D" = (xH + 0, yH - 1)

relativePosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (xH, yH) (xT, yT) = (xT - xH, yT - yH)

delta :: Int -> Int
delta x = x - (signum x) * (abs x + 1)

moveT :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveT (xH, yH) (xT, yT)
  | abs relX < 2 && abs relY < 2        = (xT + 0, yT + 0)
  | otherwise                           = (xT + delta relX, yT + delta relY) where
    (relX, relY) = relativePosition (xH, yH) (xT, yT)


move :: ((Int, Int), (Int, Int)) -> String -> ((Int, Int), (Int, Int))
move (posH, posT) m = (newPosH, newPosT) where
  newPosH = moveH posH m
  newPosT = moveT newPosH posT

solve1 :: [String] -> Int
solve1 lines = length . unique $ tailLocations where
  tailLocations = (0, 0) : map snd trajectories
  trajectories = scanl move ((0, 0), (0, 0)) lines

move2 :: [(Int, Int)] -> String -> [(Int, Int)]
move2 (posH : posTs) m = newPosH : newPosTs where
  newPosH = moveH posH m
  newPosTs = tail . scanl moveT newPosH $ posTs 

solve2 :: [String] -> Int
solve2 lines = length . unique $ tailLocations where
  tailLocations = (0, 0) : map (!! 9) trajectories
  trajectories = scanl move2 initialPos lines
  initialPos = take 10 (repeat (0, 0))

main = do
  print . solve1 . concatMap interpretLine $ dummyInput
  readInput >>= print . solve1
  print . solve2 . concatMap interpretLine $ dummyInput2
  readInput >>= print . solve2
