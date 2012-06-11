module Text.XML.Conduit.Parse.Util where

import Data.XML.Types
import Text.XML.Stream.Parse

import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary

import System.IO


parseXmlFile :: (MonadThrow m, MonadIO m) => Handle -> (Sink Event m a) -> m a
parseXmlFile h iter = sourceHandle h $$ parseBytes def =$ iter 
-- do 
--   run_ $ enumHandle 4096 h $$ joinI $ parseBytes def $$ iter
--  run_ $ enumHandle 4096 h $$ joinI $ parseBytes decodeEntities $$ iter

--  x <- liftIO $ run_ $ enumFile fn $$ 
--  return x 
{-    x <- liftIO . S.readFile $ fn 
    run_ $ enumList 1 [x] $$ joinI $ parseBytes decodeEntities $$ iter -}
