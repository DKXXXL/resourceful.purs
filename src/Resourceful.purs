module Resouceful
       (resourcefulMain)
       where

import Control.Monad.Eff
import Data.Maybe
import HTMLgenerator(htmlGenerator0)
import RCommand(RCommand(Dir), commandAnlA,ptclpartition,argpartition,commandBckA)
newtype Path = String


--liftM' :: (a -> b) -> (forall e. Eff e a) -> (forall e. Eff e b)
liftM' f m = m `bind` (return . f)

resourcefulMain :: String ->Eff STORAGE String
resourcefulMain = (liftM' (demaybe . (foldl'' retUnino Nothing))) .
                  commandAnlA .
                  antiBlank'
  where retUnino _ (Just y) = y
        retUnino x' Nothing = x'
        demaybe (Just x) = x
        demaybe Nothing = ""
        antiBlank' x = if x == []
                       then commandBckA (Dir "/")
                       else x
        foldl'' f init [] = init
        foldl'' f init (x:y) = foldl'' f (f init x) y

