{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,  
             ExistentialQuantification #-}

module Data.Iteratee.Util where

import Data.ListLike as LL 
import Data.Iteratee.ListLike 
import Data.Iteratee as Iter

import Control.Monad.State
import Control.Monad.IO.Class


actionIO :: (MonadIO m, LL.ListLike s el, Iter.Nullable s, Show el) => (el -> IO ()) -> Iter.Iteratee s m ()
actionIO act = do chk <- Iter.isFinished
                  if chk 
                    then Iter.idone () (Iter.EOF Nothing)
                    else Iter.head >>= liftIO . act >> actionIO act


printI :: (MonadIO m, LL.ListLike s el, Iter.Nullable s, Show el) => Iter.Iteratee s m ()
printI = do chk <- Iter.isFinished
            if chk 
              then Iter.idone () (Iter.EOF Nothing)
              else Iter.head >>= liftIO . print >> printI 
              
              
printIF :: (MonadIO m, LL.ListLike s el, Iter.Nullable s, Show a) => 
          (el -> a ) -> Iter.Iteratee s m ()
printIF f = do chk <- Iter.isFinished
               if chk 
                 then Iter.idone () (Iter.EOF Nothing)
                 else Iter.head >>= liftIO . print . f >> printIF f



count_marker :: (MonadIO m) => Int -> Int -> Iteratee [x] m ()
count_marker num start = do 
  chk <- isFinished
  if chk 
    then idone () (EOF Nothing)
    else if start `mod` num == 0 
         then do liftIO $ Prelude.putStrLn (" data : " ++  show start)
                 Iter.head
                 count_marker num (start+1)
         else do Iter.head
                 count_marker num (start+1)


mapStreamM :: (Monad m) => ( a -> m b) -> Enumeratee [a] [b] m c
mapStreamM fm = eneeCheckIfDone (liftI . step) 
  where 
    step k (Chunk xs) 
      | LL.null xs = liftI (step k)
      | True       = joinIM $ do ys <- Prelude.mapM fm xs 
                                 let str = Chunk ys     
                                 return $ mapStreamM fm (k str)     
    step k s       = idone (liftI k) s 


count = Iter.length 


(<+>) = Iter.enumPair 

jn = Iter.joinI

filtre = Iter.filter 


-- | Splice a list every 100 elements
splice100 :: (Monad m) => [a] -> Enumerator [a] m b
splice100 lst = enumPureNChunk lst 100 

-- | Feed a state in the beginning of iteratee operation and run it 
runIterWithState :: Monad m => 
                    st -> (StateT st m) (Iteratee s (StateT st m) a) -> m a 
runIterWithState st x = (flip evalStateT) st $ run . joinIM $ x

  
-- | Showing progress in terms of number of events processed.  
workWithCounter :: (Monad m, MonadIO m) => 
                Iteratee [s] m a -> Iteratee [s] m (a,()) 
workWithCounter iter = iter <+> count_marker 1000 0 



--data ShowObj = forall a. (Show a) => ShowObj a

--type S a () = (a,()) 

--mkShowObjList :: (Show a, Show b, Show c) => (a,b) -> [ShowObj]