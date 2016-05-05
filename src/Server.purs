module Server where

foreign import ServerStart :: Int ->(String -> String) -> Int
