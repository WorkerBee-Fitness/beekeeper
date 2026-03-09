{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BK 
    () where

import Prelude.Linear
    ( Show,
      Bool(False, True),
      String,
      IO,
      Either,
      undefined,
      putStrLn,
      FilePath,      
      Semigroup((<>)) )

import Prelude 
    (($), 
     (.), 
     (<$>))

import System.IO (Handle, hIsEOF, withFile, IOMode (ReadMode))
import Data.Text (Text)
import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Csv.Incremental (Parser(..), decode)
import Data.Csv(FromRecord, ToRecord, HasHeader (NoHeader))

import qualified WBeeLib.ByteString as WBL
import System.Exit (exitFailure)
import Control.Monad (Monad(..), return)
import Data.Array.Mutable.Linear (Array, alloc)

data Bookmark = Bookmark {
    label :: !Text,
    target :: !Text
} deriving (Generic, Show)

instance FromRecord Bookmark
instance ToRecord Bookmark

feedCSVFile 
    :: (BS.ByteString -> Parser Bookmark)
    -> Handle 
    -> IO (Parser Bookmark)
feedCSVFile parseBK csvFile = do
    hIsEOF csvFile >>= \case 
        True -> return . parseBK $ BS.empty
        False -> parseBK <$> BS.hGetSome csvFile 4096

readCSVFile 
    :: FilePath 
    -> IO (Array (Either String Bookmark))
readCSVFile csvFilePath = do
    withFile csvFilePath ReadMode $ \csvFile ->
        let loop :: Array (Either String Bookmark) %1
                 -> Parser Bookmark 
                 -> IO (Array (Either String Bookmark))
            loop _ (Fail _ errMsg)    = putStrLn errMsg >> exitFailure
            loop acc (Many rs parseBK) = feedCSVFile parseBK csvFile
                                     >>= loop (acc <> rs)                                        
            loop acc (Done rs)         = return $ acc <> rs
        in alloc 100 (Bookmark "" "") $ \acc -> 
                loop acc (decode NoHeader)

sortBKs :: [Either String Bookmark] 
        -> Either String [Bookmark]
sortBKs = undefined