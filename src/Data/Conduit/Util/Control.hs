{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Conduit.Util.Control
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Some utility functions for conduit control
--
-----------------------------------------------------------------------------

module Data.Conduit.Util.Control where

import           Control.Applicative
-- import           Control.Monad.Maybe
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import           Data.Conduit.List as CL hiding (mapM,sequence)
import qualified Data.Conduit.Internal as CU
import qualified Data.IntMap as IM
import           Data.Void
-- 
import           Prelude hiding (dropWhile,takeWhile,sequence)

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
            (s -> Bool) 
         -> (s -> IO ())  -- ^ true action 
         -> (s -> IO ())  -- ^ false action 
         -> Sink s m ()
doBranch criterion taction faction = do 
    await >>= maybe (return ()) 
                (\c->do let b = criterion c 
                        if b 
                          then liftIO $ taction c
                          else liftIO $ faction c
                        doBranch criterion taction faction 
                )
  

-- | 
doBranchE :: (MonadIO m) => 
            (s -> Either r r') 
         -> (r -> IO ())  -- ^ left action 
         -> (r' -> IO ())  -- ^ right action 
         -> Sink s m ()
doBranchE criterion laction raction = do 
    awaitForever $ liftIO . either laction raction . criterion


---------------------------------
-- stream mapping
--------------------------------- 



-- | dropWhile for Listlike conduit 
dropWhile :: Monad m => (a -> Bool) -> Sink a m () 
dropWhile p = do mr <- await 
                 case mr of 
                   Nothing -> return () 
                   Just r -> if p r then dropWhile p else (leftover r >> return ())


    
-- | takeWhile in stream for Listlike conduit
takeWhile :: Monad m => (a -> Bool) -> Conduit a m a 
takeWhile p = 
    await >>= maybe (return ()) 
                (\r -> if p r 
                       then (yield r >> takeWhile p) 
                       else leftover r >> return ()
                )
                             
-- | takeWhile in result for Listlike conduit, returning the resultant list

takeWhileR :: Monad m => (a -> Bool) -> Sink a m [a]
takeWhileR q = go q id
  where go p front = 
          await >>= maybe (return (front []))
                      (\r -> if p r 
                               then go p (front.(r:)) 
                               else leftover r >> return (front [])
                      )

-- | make a new source zipped with a list

zipStreamWithList :: (Monad m) => [a] -> Source m s -> Source m (a,s) 
zipStreamWithList lst osrc = CU.zipSources lsrc osrc 
  where lsrc = CL.sourceList lst 


-- | take first N elements as a new conduit

takeFirstN :: Monad m => Int -> Conduit s m s 
takeFirstN n 
  | n > 0 = await >>= maybe (return ()) (\r -> yield r >> takeFirstN (n-1))
  | otherwise = return ()


-- | 

zipSinks3 :: Monad m => Sink i m r -> Sink i m r' -> Sink i m r'' -> Sink i m (r,r',r'') 
zipSinks3 s1 s2 s3 = fmap (\((x,y),z) -> (x,y,z)) (s1 `CU.zipSinks` s2 `CU.zipSinks` s3)

-- | zip a list of sources 

zipN :: Monad m => [Source m a] -> Source m [a]
zipN = foldr f z0
  where z0 = CL.sourceList (repeat [])

        f :: Monad m => Source m a -> Source m [a] -> Source m [a]
        f s1 s2 = CU.zipSources s1 s2 =$= CL.map (\(x,xs)-> x:xs) 

-- | 

getResumableSource :: (Monad m) => Source m a -> m (ResumableSource m a)
getResumableSource s =  (s $$+ return ()) >>= return . fst 
 
-- | 

zip2 :: forall m a b. (Monad m) => (Source m a,Source m b) -> Source m (a,b) 
zip2 (s1,s2) = do rs1 <- lift $ getResumableSource s1
                  rs2 <- lift $ getResumableSource s2 
                  zip2wrk (rs1,rs2)
  where zip2wrk :: (ResumableSource m a,ResumableSource m b) 
                -> Source m (a,b)
        zip2wrk (rs1,rs2) = do 
          (rs1',mr1) <- lift (rs1 $$++ await)
          (rs2',mr2) <- lift (rs2 $$++ await)
          case (,) <$> mr1 <*> mr2 of 
            Just r -> do yield r 
                         zip2wrk (rs1',rs2')
            Nothing -> return () 

-- | switch from source A or from source B by boolean condition source

switch2 :: forall m a. (Monad m) => 
           Source m Bool -> (Source m a, Source m a) -> Source m a
switch2 sw (s1,s2) = do rs1 <- lift $ getResumableSource s1 
                        rs2 <- lift $ getResumableSource s2 
                        sw =$= switch2conduit (rs1,rs2) 
  where switch2conduit :: (ResumableSource m a,ResumableSource m a) -> Conduit Bool m a 
        switch2conduit (rs1,rs2) = do 
            mf <- await
            case mf of 
              Nothing -> return ()
              Just f  -> if f 
                         then do (rs1',mr1) <- lift (rs1 $$++ await) 
                                 maybe (return ()) 
                                       (\r -> yield r >> switch2conduit (rs1',rs2))
                                       mr1 
                         else do (rs2',mr2) <- lift (rs2 $$++ await)
                                 maybe (return ())
                                       (\r -> yield r >> switch2conduit (rs1,rs2'))
                                       mr2 

{-
-- | 
switchMap :: forall m a. (Monad m) => 
             Source m Int -> [(Int,Source m a)] -> Source m a
switchMap sw lst = do rlst <- mapM getResSrcAssocList lst
                      let rmap = IM.fromList rlst
                      sw =$= (runMaybeT (swMapConduitAction rmap) >> return ())
  where getResSrcAssocList (x,y) = do 
            y' <- (lift . getResumableSource) y 
            return (x,y')
        swMapConduitAction :: IM.IntMap (ResumableSource m a) 
                           -> MaybeT (Pipe Int Int a () m) ()
        swMapConduitAction rmap = do
            k <- MaybeT await 
            rs <- MaybeT . return $ IM.lookup k rmap
            (rs1',mr1) <- (lift. lift) (rs $$++ await)
            r1 <- MaybeT . return $ mr1 
            lift (yield r1)
            let rmap' = IM.adjust (const rs1') k rmap 
            swMapConduitAction rmap'
            return ()
-}

-- | 

sequence :: Monad m => Sink a m b -> Conduit a m b
sequence s = 
    peek >>= maybe (return ())
               (const ((mapOutput absurd s >>= yield) >> sequence s))
