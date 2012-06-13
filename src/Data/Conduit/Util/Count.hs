-----------------------------------------------------------------------------
-- |
-- Module      : Data.Conduit.Util.Count
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Support to Count IO monad operations
--
-----------------------------------------------------------------------------

module Data.Conduit.Util.Count where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL

import HEP.Util.Count

-- | 

countIter :: (MonadCount m) => Sink s m Int
countIter = do 
  st <- lift getCounter 
  x <- CL.head 
  case x of 
    Nothing -> return st
    Just _ -> do
      st `seq` lift $ putCounter (st+1)
      countIter

-- | 

countMarkerIter :: (MonadCount m) => Sink s m ()
countMarkerIter = do 
  st <- lift getCounter
  when (st `mod` 1000 == 0) $
     liftIO . putStrLn $ show st ++ "th event" 
  t <- CL.head
  case t of 
    Nothing -> return ()
    Just _ -> countMarkerIter 


