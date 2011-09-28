--
-- Module      : Data.Enumerator.Util.Count
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support to Count IO monad operations
--

module Data.Enumerator.Util.Count where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Enumerator
import qualified Data.Enumerator.List as EL

import HEP.Util.Count

countIter :: (MonadCount m) => Iteratee s m Int
countIter = do 
  st <- lift getCounter 
  x <- EL.head 
  case x of 
    Nothing -> return st
    Just _ -> do
      st `seq` lift $ putCounter (st+1)
      countIter

countMarkerIter :: (MonadCount m) => Iteratee s m ()
countMarkerIter = do 
  st <- lift getCounter
  when (st `mod` 1000 == 0) $
     liftIO . putStrLn $ show st ++ "th event" 
  t <- EL.head
  case t of 
    Nothing -> return ()
    Just _ -> countMarkerIter 


