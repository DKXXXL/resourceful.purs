
module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Server (serverStart)
import Resourceful (resourcefulMain)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  return $ serverStart 8888 $ resourcefulMain . tail 
