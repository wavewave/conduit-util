module Text.XML.Enumerator.Parse.Util where

import Data.XML.Types
import Text.XML.Enumerator.Parse
import qualified Data.ByteString as S

import Control.Monad.IO.Class
import Data.Enumerator
import Data.Enumerator.Binary

import System.IO


parseXmlFile :: (MonadIO m) => Handle -> (Iteratee Event m a) -> m a
parseXmlFile h iter = do 
  run_ $ enumHandle 4096 h $$ joinI $ parseBytes decodeEntities $$ iter

--  x <- liftIO $ run_ $ enumFile fn $$ 
--  return x 
{-    x <- liftIO . S.readFile $ fn 
    run_ $ enumList 1 [x] $$ joinI $ parseBytes decodeEntities $$ iter -}
