{-# LANGUAGE TypeSynonymInstances #-}

module Data.Enumerator.Util where

import Control.Monad
import Control.Monad.Trans

import Data.Enumerator 
import Data.Monoid 
 
import Prelude hiding (head)

enumWholeChunk :: (Monad m) => [s] -> Enumerator s m a
enumWholeChunk ys (Continue cont) = cont (Chunks ys) 
enumWholeChunk ys (Yield b xs) = yield b (xs `mappend` Chunks ys)
enumWholeChunk _ys (Error err) = returnI (Error err)  

enumZip 
  :: (Monad m) -- , MonadIO m)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
enumZip x y = continue step
  where
     -- step :: Stream s -> Iteratee s m (a,b)
    step (Chunks []) = continue step
    step (Chunks str) = do 
      sx <- (lift.runIteratee) . (enumWholeChunk str) =<< (lift.runIteratee) x
      sy <- (lift.runIteratee) . (enumWholeChunk str) =<< (lift.runIteratee) y  
      case (sx,sy) of 
        (Continue cx, Continue cy) -> enumZip (continue cx) (continue cy)
        (Yield rx _xstr, Continue cy) -> enumZip (yield rx (Chunks [])) (continue cy)
        (Continue cx, Yield ry _ystr) -> enumZip (continue cx) (yield ry (Chunks []))
        (Yield rx xstr, Yield ry ystr) -> yield (rx,ry) (shorter xstr ystr)  
        (Error e, _) -> returnI (Error e)
        (_, Error e) -> returnI (Error e)
    step EOF = do 
      sx <- lift $ runIteratee x
      sy <- lift $ runIteratee y 
      liftM2 (,) (enumEOF sx) (enumEOF sy)
    shorter c1@(Chunks xs) c2@(Chunks ys)
      | Prelude.length xs < Prelude.length ys = c1
      | otherwise             = c2
    shorter EOF _ = EOF
    shorter _ EOF = EOF
{-# INLINE enumZip #-}




{-
import Data.ListLike as LL 
import Data.Iteratee.ListLike 
import Data.Iteratee as Iter

import Control.Monad.State hiding (liftIO, MonadIO)
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

(<:>) :: (Monad m) => Iter.Iteratee [s] m a -> Iter.Iteratee [s] m [a] 
         -> Iter.Iteratee [s] m [a]
(<:>) iter1 iter2 = do 
  (x,xs) <- Iter.enumPair iter1 iter2 
  return (x:xs)


jn = Iter.joinI

filtre = Iter.filter 

-- | iterTransformer version of Iteratee.take 
takeFirstNElem :: (Monad m, MonadIO m, LL.ListLike s el, Nullable s) =>
             Int -> Iteratee s m a -> Iteratee s m a 
takeFirstNElem n = jn . (Iter.take n)


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


tupleToList2 ((n1,n2),()) = [n1,n2]

tupleToList3 (((n1,n2),n3),()) =[n1,n2,n3]

{-
tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]

tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]

tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]

tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]

tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]

tupletolist9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9] -}


tupleToList9 (((((((((n1,n2),n3),n4),n5),n6),n7),n8),n9),())
  = [n1,n2,n3,n4,n5,n6,n7,n8,n9]
-}

