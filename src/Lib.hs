module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    , findWordInCellLinePrefix
    , skew
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell (Cell, Indent)
    , Game (gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes, listToMaybe)
import qualified Data.Map as M

data Game = Game { 
                gameGrid :: Grid Cell,  
                gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show


data Cell = Cell (Integer, Integer) Char 
            | Indent 
                deriving (Eq, Ord, Show)

type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game 
makeGame grid words = 
    let gwc = gridWithCoords grid 
        tuplefy word = (word, Nothing)
        list = map tuplefy words
        dict = M.fromList list
    in Game gwc dict

totalWords :: Game -> Int 
totalWords game = length . M.keys $ gameWords game 

score :: Game -> Int 
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool 
completed game = score game == totalWords game 

playGame :: Game -> String -> Game 
playGame game word | not $ M.member word (gameWords game) = game 
playGame game word = 
    let grid = gameGrid game 
        foundWord = findWord grid word
    in case foundWord of 
        Nothing -> game 
        Just cs -> 
            let dict = gameWords game 
                newDict = M.insert word foundWord dict 
            in game { gameWords = newDict}
    

formatGame :: Game -> String 
formatGame game = 
    let grid = gameGrid game 
    in  formatGrid grid 
        ++ "\n\n"
        ++ (show $ score game)
        ++ "/"
        ++ (show $ totalWords game)

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b 
mapOverGrid = map . map

cell2char :: Cell -> Char 
cell2char (Cell _ c) = c
cell2char Indent = '?'

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols  

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

outputGrid :: Grid Cell -> IO()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char 
   

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
    let horizontal = grid 
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ map reverse lines

diagonalize :: Grid Cell -> Grid Cell
--diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew 

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs) 
    where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
-- findWord grid word = or $ map (findWordInLine word) grid
findWord grid word = 
    let lines = getLines grid -- We've refactored and abstracted out `grid ++ (map reverse grid)`
        foundWords = map (findWordInLine word) lines
    in listToMaybe (catMaybes foundWords)
    --in or $ map (findWordInLine word) lines
    --    found = or $ map (findWordInLine word) lines 
    --in if found then Just word else Nothing

findWords :: Grid Cell -> [String] -> Maybe [Cell]
findWords grid words = 
    let foundWords = map (findWord grid) words 
    in listToMaybe (catMaybes foundWords)

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line = --isInfixOf
    let found = findWordInCellLinePrefix [] word line 
    in case found of 
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c 
    = findWordInCellLinePrefix (c : acc) xs cs 
findWordInCellLinePrefix acc [] _ = Just $ reverse acc 
findWordInCellLinePrefix _ _ _ = Nothing  


