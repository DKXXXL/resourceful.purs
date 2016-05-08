module RCommand
       (argpartition,ptclpartition,RCommand, commandAnl, commandBck, commandBckA)
       where

import Control.Monad.Eff
import Data.Maybe
import RCommandOp (rcOpf)

type Path = String

ptclpartition = "&"
argpartition = ","

data RCommand = Dir Path
              | Copy {form :: Path, to :: Path}
              | Mkdir Path
              | Remove Path
              | Move {from :: Path, to :: Path}
--              | Rename {from :: Path, to :: Path}
              | ErrorC


commandAnl :: Array String -> RCommand              
commandAnl ["dir",x] = Dir x
commandAnl ["copy",x,y] = Copy x y
commandAnl ["mkdir",x,] = Mkdir x
commandAnl ["remove",x] = Remove x
commandAnl ["move",x,y] = Move x y
--commandAnl ("rename":x:y:[]) = Remove x y
commandAnl _ = ErrorC


fc :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
fc f g = \x -> (f (g x))

commandAnlA :: String -> Eff STORAGE (Array (Maybe String))
commandAnlA = desugar `fc` (map (commandReturn' `fc` runCommand `fc` parser3 `fc` parser2) ) `fc` parser1'
  where desugar :: Array (Eff STORAGE (Maybe String)) -> Eff STORAGE (Array (Maybe String))
        desugar (x:y) = x `bind` (\x' -> ((desugar y) `bind` (\y' ->return $ x' : y')))
        desugar [] = return []
        
commandBck :: RCommand -> Array String
commandBck (Dir x) = "dir":x:[]

commandBckA :: RCommand -> String
commandBckA  = (\x -> cmdGenerator (head x) (tail x) )`fc` commandBck 

cmdGenerator :: String -> Array String -> String
cmdGenerator = foldr (\x y -> (x ++ argpartition ++ y)) 


split x y = case (find' x y) of [a] -> a : []
                                [a , b]  -> a : (find b y)
  where find'  (k:x') y = if(length(y) > length ((k:x')))
                          then (k:x' : []) 
                          else if (y `isPrefixof` (k:x'))
                               then (y: (prefixof (k:x') (length y)):[])
                               else find' x' y
        prefixof x 0 = x
        prefixof (_:x') n = prefixof x' (n - 1)


parser1' :: String -> Array String
parser1' str = split str ptclpartition

parser2 :: String -> Array String
parser2 str = split str argpartition

        
parser3 :: Array String -> RCommand
parser3 = commandAnl


runCommand :: RCommand -> Eff STORAGE (Maybe String)
runCommand (Dir x) = rcOp (getrootfile x) "dir" (x:[])
runCommand (Copy x y) = rcOp (getrootfile x) "copy" (x:y:[])
runCommand (Mkdir x) = rcOp (getrootfile x) "mkdir" (x:[])
runCommand (Remove x) = rcOp (getrootfile x) "remove" (x:[])
runCommand (Move x y) = rcOp (getrootfile x) "move" (x:y:[])
  where getrootfile x = find'' '/' x []
        find'' :: Char -> String -> String -> String
        find'' x (x:t) ret = reverse ret []
        find'' x (y:t) ret = find'' x t (y:ret)
        reverse (x:y) z = reverse y (x:z)
        reverse [] y = y
commandReturn' :: Eff STORAGE (Maybe String) -> EFF STORAGE (Maybe String)
commandReturn' x = x `bind` (\y -> return $ commandReturn'' y)

commandReturn'' :: Maybe String -> Maybe String
commandReturn'' x = x `bind` (\y -> return $ commandReturn y)

commandReturn :: String -> String
commandReturn str = (\x ->htmlShow $ htmlGenerator0 (head x) (tail x)) $ split str ptclpartition



rcOp :: String -> String -> Array String -> forall e. Eff (storage :: STORAGE | e) String
rcOp rootfile funcname funcargvs = rcOpf rootfile (call' funcname funcargvs)
  where call' funcname funcargvs = funcname ++ ("(" ++ (call'' funcargvs))
        call'' :: Array String -> String
        call'' (x:[]) = x ++ ")"
        call'' (x:y) = x ++ (',' ++ (call'' y))
