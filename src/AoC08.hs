module AoC08 where

import Data.Functor
import Data.List(transpose)
import Data.Bifunctor(first)
import Data.Char(digitToInt)

readInput :: IO [[Int]]
readInput = readFile "input/08" 
    <&> lines
    <&> fmap (map digitToInt)

visible'' :: ([Int], Int) -> ([Int], Int)
visible'' ([], m) = ([], m)
visible'' (h : hs, m) = first ((if h > m then 1 :: Int else 0 :: Int) :) (visible'' (hs, max m h))

visible' :: [Int] -> [Int]
visible' hs = fst $ visible'' (hs, -1)

visible :: [[Int]] -> [[Int]]
visible = map visible'

anyVisible :: [[Int]] -> [[Int]] -> [[Int]]
anyVisible = zipWith (zipWith (\x y -> if x + y > 0 then 1 else 0))

getInner :: [a] -> [a]
getInner = init . tail 

solve1 :: [[Int]] -> Int
solve1 input = sum . map sum $ v where
  v = foldl1 anyVisible [rowVisible1, rowVisible2, colVisible1, colVisible2]
  rowVisible1 = visible input
  rowVisible2 = map reverse. visible . map reverse $ input
  colVisible1 = transpose . visible . transpose $ input
  colVisible2 = transpose . map reverse . visible . map reverse . transpose $ input

numVisibleFromHeight' ::  Int -> Int -> [Int] -> [Int] 
numVisibleFromHeight'  _ _ [] = []
numVisibleFromHeight' tally height (tree : trees) = newTally : numVisibleFromHeight' newTally height trees where
    newTally = 1 + if tree >= height then 0 else tally

numVisibleFromHeight :: Int -> [Int] -> [Int] 
numVisibleFromHeight height trees =  0 : numVisibleFromHeight' 0 height (init trees)

numVisibleFromHeightForest :: Int -> [[Int]] -> [[Int]]
numVisibleFromHeightForest height = map (numVisibleFromHeight height)

productVisibility :: [[Int]] -> [[Int]] -> [[Int]]
productVisibility = zipWith (zipWith (*))

visibilityMap :: Int -> [[Int]] -> [[Int]]
visibilityMap height forest = foldl1 productVisibility maps where
  maps = [mapFromLeft, mapFromRight, mapFromTop, mapFromBottom]
  mapFromLeft = numVisibleFromHeightForest height forest
  mapFromRight = map reverse . numVisibleFromHeightForest height . map reverse $ forest
  mapFromTop = transpose . numVisibleFromHeightForest height . transpose $ forest
  mapFromBottom = transpose . map reverse . numVisibleFromHeightForest height . map reverse . transpose $ forest

visibilityScore :: [[Int]] -> Int -> Int -> Int
visibilityScore forest x y = (visibilityScores !! x) !! y where
  visibilityScores = visibilityMap height forest
  height = (forest !! x) !! y

solve2 :: [[Int]] -> Int
solve2 forest = maximum . map maximum $ visibilityScores where
  visibilityScores = map (\y -> map ($ y) funcs) ys
  funcs = map (visibilityScore forest) xs
  xs = [0..(length forest - 1)]
  ys = [0..((length . head $ forest) - 1)]

-- --solve2 forest = visibilityScore forest 3 2
-- solve2 forest = numVisibleFromHeightForest 5 forest

dummyInput = 
  [ [3, 0, 3, 7, 3]
  , [2, 5, 5, 1, 2]
  , [6, 5, 3, 3, 2]
  , [3, 3, 5, 4, 9]
  , [3, 5, 3, 9, 0]]

main = do
  print . solve1 $ dummyInput
  readInput >>= print . solve1
  print . solve2 $ dummyInput
  readInput >>= print . solve2
--  print [1,2,3]