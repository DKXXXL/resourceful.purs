module RCommand
       (RCommand, commandAnl, commandBck, commandBckA)
       where

ptclpartition = "|||"
argpartition = ","

data RCommand = Dir Path
              | Copy {form :: Path, to :: Path}
              | Mkdir Path
              | Remove Path
              | Move {from :: Path, to :: Path}
              | Rename {from :: Path, to :: Path}
              | ErrorC


commandAnl :: [String] -> RCommand              
commandAnl ("dir":x:[]) = Dir x
commandAnl ("copy":x:y:[]) = Copy x y
commandAnl ("mkdir":x:[]) = Mkdir x
commandAnl ("remove":x:[]) = Remove x
commandAnl ("move":x:y:[]) = Move x y
commandAnl ("rename":x:y:[]) = Remove x y
commandAnl _ = ErrorC

commandBck :: RCommand -> [String]
commandBck (Dir x) = "dir":x:[]

commandBckA :: RCommand -> String


cmdGenerator :: String -> [String] -> String
cmdGenerator = foldr (++ argpartition ++) 
