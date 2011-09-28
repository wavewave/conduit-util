--
-- Module      : Data.Enumerator.Util.Control
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Some Control Iteratee 
--

module Data.Enumerator.Util.Control where

import Data.Enumerator
import qualified Data.Enumerator.List as EL
import Control.Monad.IO.Class

doNIter :: (MonadIO m) => Int -> (s -> IO ()) -> Iteratee s m () 
doNIter n action
  | n > 0 = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just c -> do 
        liftIO $ action c
        doNIter (n-1) action
  | otherwise = return () 

doBranch :: (MonadIO m) => 
            (s -> (Bool,s')) 
             -> (s' -> IO ()) 
             -> (s' -> IO ()) 
             -> Iteratee (Maybe s) m ()
doBranch criterion taction faction = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just maybec -> do 
        case maybec of 
          Just c -> do 
            let (b,c') = criterion c 
            if b 
              then liftIO $ taction c'
              else liftIO $ faction c'
          Nothing -> do 
            liftIO $ putStrLn "what?"
            return ()
        doBranch criterion taction faction 
  
---------------------------------
-- stream mapping
--------------------------------- 

takeEnee :: (Monad m) => Int -> Enumeratee s s m a
takeEnee n = checkDone (continue . (step n)) where
  step _ k EOF = yield (Continue k) EOF
  step num k (Chunks xs) = loop num k xs 

  loop num k [] | num > 0 = continue (step num k)
  loop num k (x:xs) | num > 0 = do 
    k (Chunks [x]) >>== 
      checkDoneEx (Chunks xs) (\k' -> loop (num-1) k' xs)
  loop _ k _ | otherwise = yield (Continue k) EOF





