--
-- Module      : Data.Enumerator.Util.Print
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Print support in Iteratee
--

module Data.Enumerator.Util.Print where

import Control.Monad
import Control.Monad.IO.Class

import Data.Enumerator 
import qualified Data.Enumerator.List as EL
import Data.Enumerator.Util.Control

printIter :: (MonadIO m, Show s) => Iteratee s m () 
printIter = do 
  elm <- EL.head
  case elm of 
    Nothing -> return ()
    Just c -> do 
      liftIO . putStrLn $ show c
      printIter 

printNIter :: (MonadIO m, Show s) => Int -> (s -> String) -> Iteratee s m () 
printNIter n formatter = doNIter n (putStrLn . formatter)

printSatisfying :: (MonadIO m, Show s) => (s->Bool) -> Iteratee s m ()
printSatisfying f = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just c -> do 
      when (f c) $ do 
        liftIO $ putStrLn "------------------"
        liftIO . putStrLn . show $ c
      printSatisfying f  

