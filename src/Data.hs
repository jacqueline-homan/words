module Data (grid
            , languages
            ) where

-- copy of word grid
grid :: [String]
grid = [  "__C________R___"
        , "__SI________U__" 
        , "__HASKELL____B_"
        , "__A__A_____S__Y"
        , "__R___B___C____"
        , "__PHP____H_____"
        , "____S_LREP_____"
        , "____I__M_Y__L__"
        , "____L_E__T_O___"
        , "_________HB____"
        , "_________O_____"
        , "________CN_____"
       ]
-- finite list of included language names in the word grid
languages :: [String]
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]

