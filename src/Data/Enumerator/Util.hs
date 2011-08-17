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
  :: (Monad m) 
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
enumZip x y = continue step
  where
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


enumZip3 :: (Monad m) => Iteratee s m a -> Iteratee s m b -> Iteratee s m c 
            -> Iteratee s m (a,b,c)
enumZip3 a b c = enumZip a (enumZip b c) >>= \(r1,(r2,r3)) -> return (r1,r2,r3)

enumZip4 :: (Monad m) => Iteratee s m a 
         -> Iteratee s m b 
         -> Iteratee s m c 
         -> Iteratee s m d  
         -> Iteratee s m (a,b,c,d)
enumZip4 a b c d = enumZip a (enumZip3 b c d) >>= \(r1,(r2,r3,r4)) -> return (r1,r2,r3,r4)

