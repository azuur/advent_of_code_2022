module Main where

import qualified AoC01
import qualified AoC02
import qualified AoC03


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
  putStrLn "Day 3"
  AoC03.main
  

--module Main where

--import qualified MyLib (someFunc)

--main :: IO ()
--main = do
--  putStrLn "Hello, Haskell!"
--  MyLib.someFunc
--module MyLib (someFunc) where

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"