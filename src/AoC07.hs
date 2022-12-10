module AoC07 where

import Data.Functor
import Data.List.Split(splitWhen)
import qualified Data.Map.Strict as Map

dummyInput = 
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]

readInput :: IO [String]
readInput = readFile "input/07" <&> lines

data FSObject = Dir String | File String Int deriving (Show, Eq)

data FileSystem = NullFS | FS FSObject FileSystem FileSystem deriving (Show, Eq) -- Null OR (self, sibling, child) 
--- A binary tree, thanks to https://stackoverflow.com/a/41409851 for the idea

type Path = [FSObject] -- ex: [Dir "/", Dir "home", dir "Documents", File "cool_doc.txt" 5]

cdUp :: Path -> Path
cdUp = init

cdDown :: Path -> FSObject -> Path
cdDown fs f = fs ++ [f]

root :: FSObject
root = Dir "/"

baseFS :: FileSystem
baseFS = FS root NullFS NullFS

getFSFromRelativePath :: FileSystem -> Path ->  FileSystem  -- get subtree of FS given by last node in path
getFSFromRelativePath fs [] = fs -- nothing left to check, return whatever is here
getFSFromRelativePath NullFS _ = NullFS -- file/dir not found (nothing else in current dir to check)
getFSFromRelativePath (FS self sibling child) (currentNode : restOfPath)  
  | self == currentNode && null restOfPath          = FS self sibling child  -- found the final dir/file
  | self == currentNode                             = getFSFromRelativePath child restOfPath  -- found the next dir/file, look in children
  | otherwise                                       = getFSFromRelativePath sibling (currentNode : restOfPath) -- dir/file not found yet, check siblings 

calculateSize' :: Bool -> FileSystem -> Int  --- first arg is whether to calculate Size in siblings, too
calculateSize' _ NullFS = 0
calculateSize' True (FS (File _ x) sibling child) = x + calculateSize' True sibling
calculateSize' True (FS (Dir _) sibling child) = calculateSize' True sibling + calculateSize' True child
calculateSize' False (FS (File _ x) sibling child) = x
calculateSize' False (FS (Dir _) sibling child) = calculateSize' True child

calculateSize :: FileSystem -> Int
calculateSize = calculateSize' False

calculateSizeDir :: FileSystem -> Path -> Int
calculateSizeDir fs = calculateSize . getFSFromRelativePath fs

insertFSObject :: FileSystem -> Path -> FileSystem
insertFSObject fs [] = fs -- nothing else to insert, return self 
insertFSObject NullFS path = FS (head path) NullFS (insertFSObject NullFS (tail path)) -- hit empty spot, insert path from here 
insertFSObject (FS self sibling child) path 
  | self == head path       = FS self sibling (insertFSObject child (tail path)) -- found next path node, keep following path to children
  | otherwise               = FS self (insertFSObject sibling path) child -- current node not found, check sibling

interactWithFS :: (FileSystem, Path) -> String -> (FileSystem, Path) 
interactWithFS (fs, path) str 
  | "$ cd /" == str             = (fs, root)
  | "$ cd .." == str            = (fs, pathUp)
  | "$ cd" == take 4 str        = (fs, pathDown) 
  | "$ ls" == take 4 str        = (fs, path)
  | "dir " == take 4 str        = (fsAddDir, path)
  | otherwise                   = (fsAddFile, path) where
    root  = [Dir "/"]
    pathUp = cdUp path
    dir = Dir (last strSplit)
    pathDown = cdDown path dir
    fsAddDir = insertFSObject fs pathDown 
    strSplit = splitWhen (==' ') str
    file = File (last strSplit) ((read :: String -> Int) (head strSplit))
    fsAddFile = insertFSObject fs (path ++ [file]) 

getAllPaths :: FileSystem -> [Path]
getAllPaths NullFS = []
getAllPaths (FS (File _ _) sibling child) = getAllPaths sibling
getAllPaths (FS (Dir x) sibling child) = [[Dir x]] ++ getAllPaths sibling ++ map ([Dir x] ++) (getAllPaths child)


testFS 
  = FS 
      (Dir "/") 
      NullFS 
      (FS 
        (Dir "a/") 
        (FS 
          (Dir "b/") 
          NullFS 
          NullFS) 
        (FS 
          (File "a_text_1.txt" 5) 
          (FS 
            (Dir "sub_a/") 
            (FS 
              (File "a_text_2.txt" 10) 
              NullFS 
              NullFS) 
            NullFS) 
          NullFS)
      )

processInput :: [String] -> ([Int], [Path], FileSystem)
processInput strs = (dirSizes, allPaths, fs) where
    dirSizes = map (calculateSizeDir fs) allPaths
    allPaths = getAllPaths fs
    (fs, _) = foldl interactWithFS (baseFS, [root]) strs

solve1 :: [String] -> Int
solve1 strs = sum . filter (<= 100000) $ dirSizes where
    (dirSizes, _, _) = processInput strs


--solve2 :: [String] -> Int
solve2 strs = answer where
    (dirSizes, allPaths, fs) = processInput strs
    totalMemory = 70000000
    usedMemory = fst . head . filter (\(_, y) -> y == [Dir "/"]) $ (zip dirSizes allPaths)
    unusedMemory = totalMemory - usedMemory
    targetUnusedMemory = 30000000
    memoryToFree = targetUnusedMemory - unusedMemory
    answer = minimum $ filter (>=memoryToFree) dirSizes
--    answer = memoryToFree   

main = do
  print $ solve1 dummyInput
  readInput >>= print . solve1
  print $ solve2 dummyInput
  readInput >>= print . solve2