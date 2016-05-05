module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Server (ServerStart)
import Resourceful (resourcefulMain)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  return $ ServerStart 8888 $ resourcefulMain . tail 
