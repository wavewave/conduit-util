module Text.XML.Conduit.Parse.Util where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Data.XML.Types
import System.IO
import Text.XML.Stream.Parse

parseXmlFile :: (MonadThrow m, MonadIO m) => Handle -> (Sink Event m a) -> m a
parseXmlFile h iter = sourceHandle h $$ parseBytes def =$ iter 
