module Lib
    ( someFunc
    , someString
    ) where

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"

grid = [ "__C________R___"
		, "__SI________U__" 
		, "__HASKELL____B_"
		]


