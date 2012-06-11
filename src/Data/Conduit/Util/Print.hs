--
-- Module      : Data.Conduit.Util.Print
-- Copyright   : (c) 2011,2012 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Print support in Conduit
--

module Data.Conduit.Util.Print where

import Control.Monad
import Control.Monad.IO.Class

import Data.Conduit 
import qualified Data.Conduit.List as CL
import Data.Conduit.Util.Control

printIter :: (MonadIO m, Show s) => Sink s m () 
printIter = do 
  elm <- CL.head
  case elm of 
    Nothing -> return ()
    Just c -> do 
      liftIO . putStrLn $ show c
      printIter 

printNIter :: (MonadIO m, Show s) => Int -> (s -> String) -> Sink s m () 
printNIter n formatter = doNIter n (putStrLn . formatter)

printSatisfying :: (MonadIO m, Show s) => (s->Bool) -> Sink s m ()
printSatisfying f = do 
  elm <- CL.head 
  case elm of 
    Nothing -> return () 
    Just c -> do 
      when (f c) $ do 
        liftIO $ putStrLn "------------------"
        liftIO . putStrLn . show $ c
      printSatisfying f  

