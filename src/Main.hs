module Main where

import qualified AoC01
import qualified AoC02
import qualified AoC03
import qualified AoC04
import qualified AoC05
import qualified AoC06


--solutions :: [IO ()]
--solutions =
--  [ Adv01.main
--  ]

main :: IO()
main = do 
  putStrLn "Hello, Haskell!"
--  putStrLn "Day 1"
--  AoC01.main --Adv01.main
--  putStrLn "Day 2"
--  AoC02.main
--  putStrLn "Day 3"
--  AoC03.main
--  putStrLn "Day 4"
--  AoC04.main
--  putStrLn "Day 5"
--  AoC05.main
  putStrLn "Day 6"
  AoC06.main
  

--module Main where

--import qualified MyLib (someFunc)

--main :: IO ()
--main = do
--  putStrLn "Hello, Haskell!"
--  MyLib.someFunc
--module MyLib (someFunc) where

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"