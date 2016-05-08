module RCommandOp (rcOpf,STORAGE) where


import Control.Monad.Eff
--import Control.Monad.Eff.Storage (STORAGE)

foreign import data STORAGE :: !

foreign import rcOpf :: String -> String ->  forall e. Eff (storage :: STORAGE | e) String  
