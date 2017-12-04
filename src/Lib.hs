module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    --, getLines
    , skew
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes)
import Data

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines  

outputGrid :: Grid -> IO()
outputGrid grid = putStrLn (formatGrid grid)

getLines :: Grid -> [String]
getLines grid = 
    let horizontal = grid 
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
--diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew 

skew :: Grid -> Grid
skew [] = []
skew (x:xs) = x : skew (map indent xs) 
    where indent line = '_' : line


findWord :: Grid -> String -> Maybe String
-- findWord grid word = or $ map (findWordInLine word) grid
findWord grid word = 
    let lines = getLines grid -- We've refactores and abstracted out `grid ++ (map reverse grid)`
    --in or $ map (findWordInLine word) lines
        found = or $ map (findWordInLine word) lines 
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words = 
    let foundWords = map (findWord grid) words 
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf


