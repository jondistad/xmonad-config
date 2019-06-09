module JExt.StrictIO ( readFile'
                     , hGetContents'
                     ) where

import System.IO
import Control.Monad.IO.Class

readFile' :: (MonadIO io) => FilePath -> io String
readFile' path = liftIO (readFile path) >>= \s -> length s `seq` return s

hGetContents' :: (MonadIO io) => Handle -> io String
hGetContents' h = liftIO (hGetContents h) >>= \s -> length s `seq` return s
