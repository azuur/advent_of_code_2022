module AoC11 where

import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)
import Data.List.Split(splitOn)
import Data.List(sort)

dummyInput =
    [ "Monkey 0:"
    , "  Starting items: 79, 98"
    , "  Operation: new = old * 19"
    , "  Test: divisible by 23"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 1:"
    , "  Starting items: 54, 65, 75, 74"
    , "  Operation: new = old + 6"
    , "  Test: divisible by 19"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 0"
    , ""
    , "Monkey 2:"
    , "  Starting items: 79, 60, 97"
    , "  Operation: new = old * old"
    , "  Test: divisible by 13"
    , "    If true: throw to monkey 1"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 3:"
    , "  Starting items: 74"
    , "  Operation: new = old + 3"
    , "  Test: divisible by 17"
    , "    If true: throw to monkey 0"
    , "    If false: throw to monkey 1"
    ]

readInput :: IO [String]
readInput = readFile "input/11" <&> lines

data Monkey = Monkey
    { monkeyId          :: Int
    , items             :: [Item]
    , worryRule         :: Int -> Int
    , numDivRule        :: Int
    , trueTarget        :: Int
    , falseTarget       :: Int
    , numInspectedItems :: Int
    }

data Item = SimpleNum Int | NumsModulo (Map.Map Int Int) deriving (Show)


parseMonkey :: [String] -> Monkey
parseMonkey strings = Monkey
  { monkeyId            = parseMonkeyId (strings !! 0)
  , items               = parseItems (strings !! 1)
  , worryRule           = parseWorryRule (strings !! 2)
  , numDivRule          = parseNumDivRule (strings !! 3)
  , trueTarget          = parseTarget (strings !! 4)
  , falseTarget         = parseTarget (strings !! 5)
  , numInspectedItems   = 0
  }

parseMonkeyId :: String -> Int
parseMonkeyId = (read :: String -> Int) . init . last . splitOn " "

parseItems :: String -> [Item]
parseItems string = items where
  itemsStr = last . splitOn ": " $ string
  items = map (SimpleNum . (read :: String -> Int) . takeWhile (/=',')) (splitOn " " itemsStr)

parseOperation :: String -> Int -> Int -> Int
parseOperation "*" = (*)
parseOperation "+" = (+)

parseExpression :: String -> Int -> Int
parseExpression "old" = Prelude.id
parseExpression x = \y -> (read :: String -> Int) x

parseWorryRule :: String -> Int -> Int
parseWorryRule string = \x -> operation (expression1 x) (expression2 x) where
  splitted = splitOn " " string
  expression2 = parseExpression (last splitted)
  operation = parseOperation (last . init $ splitted)
  expression1 = parseExpression (last . init . init $ splitted)

divisibleBy :: Int -> Int -> Bool
divisibleBy n = (==0) . (`mod` n)

parseNumDivRule :: String -> Int
parseNumDivRule = (read :: String -> Int) . last . splitOn " "

parseTarget :: String -> Int
parseTarget = (read :: String -> Int) . last . splitOn " "

readMonkeys :: [String] -> Map.Map Int Monkey
readMonkeys strings = Map.fromList (zip ids monkeys) where
  monkeys = map parseMonkey . splitOn [""] $ strings
--   divsMod = map numDivRule monkeys'
--   monkeys = map (\m -> m {items = map (fillDivsModItem divsMod) $ items m}) monkeys'
  ids = map monkeyId monkeys 

turn :: Int -> Map.Map Int Monkey -> Int -> Map.Map Int Monkey
turn divisor monkeys monkeyId 
  | null thisItems      = monkeys
  | otherwise           = turn divisor newMonkeys monkeyId
    where
      thisMonkey = fromJust $ Map.lookup monkeyId monkeys
      thisItems = items thisMonkey
      (item : otherItems) = thisItems
      newItem = updateItem thisMonkey divisor item
      worry = getWorry thisMonkey newItem
      targetId = if divisibleBy (numDivRule thisMonkey) worry then trueTarget thisMonkey else falseTarget thisMonkey
      targetMonkey = fromJust $ Map.lookup targetId monkeys
      newThisMonkey = thisMonkey { items = otherItems, numInspectedItems = 1 + numInspectedItems thisMonkey }
      newTargetMonkey = targetMonkey { items = items targetMonkey ++ [newItem] }
      newMonkeys = Map.insert monkeyId newThisMonkey . Map.insert targetId newTargetMonkey $ monkeys 

roundm :: Int -> Map.Map Int Monkey -> Map.Map Int Monkey
roundm divisor monkeys = foldl (turn divisor) monkeys [0..(Map.size monkeys - 1)]

bobito :: Int -> Int -> Map.Map Int Monkey -> Map.Map Int Monkey
bobito _ 0 monkeys = monkeys
bobito divisor n monkeys = bobito divisor (n - 1) (roundm divisor monkeys)

monkeyBusiness :: Map.Map Int Monkey -> Int
monkeyBusiness = product . take 2 . reverse . sort . map (numInspectedItems . snd) . Map.toList

solve1 :: [String] -> Int
solve1 = monkeyBusiness . bobito 3 20 . readMonkeys

simpleItemToModItem :: [Int] -> Item -> Item
simpleItemToModItem _ (NumsModulo m) = NumsModulo m
simpleItemToModItem divisors (SimpleNum x) = NumsModulo . Map.fromList . map (\d -> (d, x `mod` d)) $ divisors

getWorry :: Monkey -> Item -> Int
getWorry _ (SimpleNum x) = x
getWorry monkey (NumsModulo m) = fromJust . Map.lookup (numDivRule monkey) $ m

updateItem :: Monkey -> Int -> Item -> Item
updateItem monkey divisor (SimpleNum x) = SimpleNum (worryRule monkey x `div` divisor)
updateItem monkey divisor (NumsModulo m)
  | divisor /= 1            = error "Divisor must be 1 if keeping track of nums modulo"
  | otherwise               = NumsModulo newMap where
    newMap = Map.mapWithKey (flip mod) . Map.map (worryRule monkey) $ m

withModuloItems :: Map.Map Int Monkey -> Map.Map Int Monkey
withModuloItems monkeys = Map.map changeMonkeyItems monkeys where
  divisors = Map.elems $ Map.map numDivRule monkeys
  convert = map (simpleItemToModItem divisors)
  changeMonkeyItems m = m { items = convert $ items m }

solve2 :: [String] -> Int
solve2 = monkeyBusiness . bobito 1 10000 . withModuloItems . readMonkeys

main = do
  print . solve1 $ dummyInput
  readInput >>= print . solve1
  print . solve2 $ dummyInput
  readInput >>= print . solve2
