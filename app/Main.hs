module Main where

import Lib
import Data
import System.IO 

main :: IO ()
main = do 
    --let gwc = gridWithCoords grid
    let game = makeGame grid languages 
    hSetBuffering stdout NoBuffering 
    playTurn game 




playTurn game = do
    --in outputGrid gwc
    putStrLn . formatGame $ game
    putStr "Please enter a word: "
    word <- getLine
    --putStrLn $ "You entered " ++ word
    let newGame = playGame game word 
    --putStrLn . formatGame $ newGame 
    if completed newGame then 
        putStrLn "Congratulations!"
    else 
        playTurn newGame 
