--
-- Module      : Data.Conduit.Util.Control
-- Copyright   : (c) 2011,2012 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Some Control Conduit 
--

module Data.Conduit.Util.Control where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Monoid
import Control.Monad.IO.Class
import Prelude hiding (dropWhile,takeWhile)

-- | 

doNIter :: (MonadIO m) => Int -> (s -> IO ()) -> Sink s m () 
doNIter n action
  | n > 0 = do 
    elm <- CL.head
    case elm of 
      Nothing -> return ()
      Just c -> do 
        liftIO $ action c
        doNIter (n-1) action
  | otherwise = return () 

-- | 

doBranch :: (MonadIO m) => 
            (s -> (Bool,s')) 
             -> (s' -> IO ()) 
             -> (s' -> IO ()) 
             -> Sink (Maybe s) m ()
doBranch criterion taction faction = do 
    elm <- CL.head
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

-- | dropWhile for Listlike conduit 

dropWhile :: Monad m => (a -> Bool) -> Sink a m () 
dropWhile p = 
    NeedInput push close 
  where
    push b | p b = dropWhile p 
           | otherwise = Done Nothing ()
    close = return ()  
     
-- | takeWhile for Listlike conduit

takeWhile :: Monad m => (a -> Bool) -> Conduit a m a 
takeWhile p = NeedInput push close
  where push x 
          | p x = HaveOutput (NeedInput push close) (return ()) x
          | otherwise = Done Nothing ()
        close = mempty


-- | make a new source zipped with a list

zipStreamWithList :: (Monad m) => [a] -> Source m s -> Source m (a,s) 
zipStreamWithList lst osrc = CL.zip lsrc osrc 
  where lsrc = CL.sourceList lst 

-- CL.mapAccum f lst 
--  where f [] y = ([],Nothing)
--        f (x:xs) y = (xs,Just (x,y))

-- | take first N elements as a new conduit

takeFirstN :: (Monad m) => Int -> Conduit s m s 
takeFirstN n = NeedInput (push n) close
  where push 0 x = Done Nothing () 
        push c x = HaveOutput (NeedInput (push (c-1)) close) (return ()) x
        close = mempty

{-
takeSampleNConduit :: (Monad m) => Int -> Conduit s m s
takeSampleNConduit n = 

checkDone (continue . (step n)) where
  step _ k EOF = yield (Continue k) EOF
  step num k (Chunks xs) = loop num k xs 

  loop num k [] | num > 0 = continue (step num k)
  loop num k (x:xs) | num > 0 = do 
    k (Chunks [x]) >>== 
      checkDoneEx (Chunks xs) (\k' -> loop (num-1) k' xs)
  loop _ k _ | otherwise = yield (Continue k) EOF
-}




