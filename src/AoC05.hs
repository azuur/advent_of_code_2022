module AoC05 where

import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)
import Data.List(transpose)
import Data.List.Split(splitWhen, splitOn, splitOneOf, chunksOf)
import Data.Char(digitToInt)



dummyInput = [ "    [D]    "
             , "[N] [C]    "
             , "[Z] [M] [P]"
             , " 1   2   3 "
             , ""
             , "move 1 from 2 to 1"
             , "move 3 from 1 to 3"
             , "move 2 from 2 to 1"
             , "move 1 from 1 to 2" ]


dummyInput' = processInput dummyInput

readInput :: IO (Map.Map Int String, [(Int, Int, Int)]) 
readInput = fmap (processInput . lines) (readFile "input/05")

processInput :: [String] ->  (Map.Map Int String, [(Int, Int, Int)]) 
processInput input = (getStacks top, getMoves bottom) where
  splitted = splitWhen (== "") input 
  top = init $ head splitted
  bottom = last splitted

getMovesLine :: String -> (Int, Int, Int)
getMovesLine line = (nums !! 0, nums !! 1, nums !! 2) where
  strs = filter (not . null) $ splitOneOf "fromoveto " line
  nums = map (read :: String -> Int) strs

getMoves :: [String] -> [(Int, Int, Int)]
getMoves = map getMovesLine

getMark :: String -> String
getMark = filter (`notElem` "[] ")

getStacks :: [String] -> Map.Map Int String
getStacks input = Map.fromList (zip nums stacks) where
  stacks = map (concatMap getMark) . transpose . map (chunksOf 4) $ input
  nums = [1..(1 + length stacks)]

moveCrates1 :: Int -> (String, String) -> (String, String)
moveCrates1 0 (xs, ys) = (xs, ys)
moveCrates1 n ([], ys) = ([], ys)
moveCrates1 n (x : xs, ys) = moveCrates1 (n - 1) (xs, x : ys)

moveCrates2 :: Int -> (String, String) -> (String, String)
moveCrates2 n (xs, ys) = (drop n xs, take n xs ++ ys)

moveStacks :: (Int -> (String, String) -> (String, String)) -> (Int, Int, Int) -> Map.Map Int String -> Map.Map Int String
moveStacks moveCrates (n, from, to) oldStacks = newStacks where
  newStacks = Map.insert from newFromStack . Map.insert to newToStack $ oldStacks
  (newFromStack, newToStack) = moveCrates n (oldFromStack, oldToStack)
  oldFromStack = fromJust $ Map.lookup from oldStacks
  oldToStack = fromJust $ Map.lookup to oldStacks

topCrates :: [String] -> String
topCrates = concatMap (\x -> if not . null $ x then [head x] else "")

solve1 :: (Map.Map Int String, [(Int, Int, Int)]) -> String
solve1 (initStacks, moves) = (topCrates . Map.elems) finalStacks where
  finalStacks = foldr (moveStacks moveCrates1) initStacks (reverse moves)

solve2 :: (Map.Map Int String, [(Int, Int, Int)]) -> String
solve2 (initStacks, moves) = (topCrates . Map.elems) finalStacks where
  finalStacks = foldr (moveStacks moveCrates2) initStacks (reverse moves)


main :: IO ()
main = do
  print $ solve1 dummyInput'
  readInput >>= print . solve1
  print $ solve2 dummyInput'
  readInput >>= print . solve2

--  readInput >>= print . topCrates . Map.elems . fst 
--  print (solve2 dummyInput)
--  readInput >>= print . solve2  