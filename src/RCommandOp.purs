module RCommandOp (rcOp) where

import Control.Monad.Eff

foreign import rcOpf :: String -> String ->  forall e. Eff (STORAGE | e) String  

rcOp :: String -> String -> [String] -> forall e. Eff (STORAGE | e) String
rcOp rootfile funcname funcargvs = rcOpf rootfile (call' funcname funcargvs)
  where call' funcname funcargvs = funcname ++ ('(' : (call'' funcargvs))
        call'' (x : []) = x ++ ")"
        call'' (x : y) = x ++ (',' ++ (call'' y))
