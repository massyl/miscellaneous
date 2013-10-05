module ReImplSomeIOMonadFunctions where

import System.IO.Error hiding(catch)
import Prelude hiding (catch)
import Control.Exception
import Data.List


sequence':: Monad m => [m a] -> m [a]
sequence' = foldr execSeq $ return [] where
  execSeq a b = do x  <- a
                   xs <- b
                   return (x:xs)

sequence'_:: Monad m => [m a] -> m ()
sequence'_ = foldr (>>) $ return ()

mapM':: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f xs= sequence' (map f xs)

mapM_:: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f xs = sequence'_ (map f xs)
