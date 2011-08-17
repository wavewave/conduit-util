module Text.XML.Enumerator.Parse.Util where

import Data.XML.Types
import Text.XML.Enumerator.Parse
import qualified Data.ByteString as S

import Control.Monad.IO.Class
import Data.Enumerator

parseXmlFile :: (MonadIO m) => FilePath -> (Iteratee Event m a ) -> m a
parseXmlFile fn iter = do 
    x <- liftIO . S.readFile $ fn 
    run_ $ enumList 1 [x] $$ joinI $ parseBytes decodeEntities $$ iter
