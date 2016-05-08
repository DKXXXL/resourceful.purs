module Server where

foreign import serverStart :: Int ->(String -> String) -> Int
