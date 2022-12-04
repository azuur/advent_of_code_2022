module Main where

import qualified AoC01


--solutions :: [IO ()]
--solutions =
--  [ Adv01.main
--  ]

main :: IO()
main = do 
  putStrLn "Hello, Haskell!"
  putStrLn "Day 1"
  AoC01.main --Adv01.main
  putStrLn "Day 2"
  

--module Main where

--import qualified MyLib (someFunc)

--main :: IO ()
--main = do
--  putStrLn "Hello, Haskell!"
--  MyLib.someFunc
--module MyLib (someFunc) where

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"