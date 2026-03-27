{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module BK 
    (Bookmark(..),
     BKType(..),  
     parseBKType,   
     readCSVFile,
     writeCSVFile,
     addBookmark,
     removeBookmark,
     findBookmark,
     handler,
     handler_,
     recentBookmarks,
     isAlias,
     showBKMap,
     maxOffsetBKMap) where

import Prelude.Linear
    ( Show(..),
      Bool(False, True),
      String,
      IO,
      Either,
      putStrLn,
      FilePath, otherwise, Ordering )

import Prelude
    (($),
     (.), Either (..), Foldable (..), Eq (..), Maybe (..), id, error, not, (<=), (&&), Int, Ord (..), undefined, Num (..))

import qualified Control.Functor.Linear as Linear
import qualified System.IO.Resource.Linear as Linear
import qualified Data.Unrestricted.Linear as Linear

import Data.Text (Text, concat, pack)
import qualified Data.Text as DT 

import Data.ByteString as BS hiding (foldl, null)
import GHC.Generics (Generic)
import Data.Csv.Incremental (decode, HasHeader (HasHeader))
import qualified Data.Csv.Incremental as CsvInc
import Data.Csv(FromRecord(..), ToRecord(..), FromField (..), Field, ToField (..), encodeByName, ToNamedRecord (..), namedRecord, (.=), NamedRecord, Name, Parser)
import qualified Data.Vector as Vec

import qualified WBeeLib.ByteString as WBL
import qualified WBeeLib.Text as WBL
import qualified WBeeLib.FileSystem as WBL

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (Monad(..), return, when)
import Prelude (print, MonadFail (fail))
import GHC.IO.IOMode (IOMode(..))
import qualified Data.Map as Map
import Prelude ((<>))
import Data.Bifunctor (Bifunctor(bimap))

import Data.Time.Calendar (Day, addDays)
import Data.Time.Format.ISO8601 (formatParseM, ISO8601 (iso8601Format), Format (formatShowM))
import Data.List (sortBy, head)
import Data.Ord (Down(Down))
import Data.Foldable (maximumBy)

data BKType = BKAlias 
            | BKBookmark
    deriving (Generic)

showBKType :: BKType -> Text
showBKType BKAlias    = "alias"
showBKType BKBookmark = "bookmark"

-- | The number of spaces needed to properly space `BKType`'s when pretty
-- printing bookmarks.
--
-- This is equivalent to `length (showBKType bktype) + 1`.
offsetBKType :: BKType -> Int
offsetBKType BKAlias    = 4
offsetBKType BKBookmark = 1

instance Show BKType where
    show :: BKType -> String
    show BKAlias = "alias"
    show BKBookmark = "bookmark"

instance FromField  BKType where
    parseField :: Field -> Parser BKType
    parseField = parseField' . (bimap DT.unpack id) . parseBKType . WBL.byteStringToTextUTF8
        where
            parseField' (Right bt) = return bt
            parseField' (Left err) = fail err

parseBKType 
    :: Text
    -> Either Text BKType
parseBKType s 
        | s == "alias"    = return BKAlias
        | s == "bookmark" = return BKBookmark
        | otherwise       = Left $ s <> " is not a valid bookmark type"

instance ToField BKType where
  toField :: BKType -> Field
  toField BKAlias = "alias"
  toField BKBookmark = "bookmark"

instance FromField Day where
    parseField :: Field -> Parser Day
    parseField s = 
        case formatParseM iso8601Format (WBL.byteStringToStringUTF8 s) of
            Nothing -> _fail $ s <> " is not a valid ISO8601 date"
            Just day -> return day
        where
            _fail = fail . WBL.byteStringToStringUTF8

instance ToField Day where
    toField :: Day -> Field
    toField day = 
        case formatShowM iso8601Format day of
            Nothing -> error "[toField]: failed to pretty print day"
            Just s -> WBL.stringUTF8ToByteString s

data Bookmark = Bookmark {
    bkType     :: !BKType,
    bkLabel    :: !Text,    
    bkTarget   :: !Text,
    bkCreated  :: !Day,
    bkLastUsed :: !Day
} deriving (Generic)

instance Show Bookmark where
    show :: Bookmark -> String  
    show (Bookmark bkT bkL bkTar _ _) 
        = (show bkT) <> " " <> (DT.unpack bkL) <> "=" <> (show bkTar) 

instance FromRecord Bookmark
instance ToRecord   Bookmark

header :: Vec.Vector Name
header = Vec.fromList 
    ["type",
     "label",
     "target",
     "created-data",
     "last-used"]

instance ToNamedRecord Bookmark where
    toNamedRecord :: Bookmark -> NamedRecord
    toNamedRecord b = namedRecord [
        "type" .= bkType b, 
        "label" .= bkLabel b, 
        "target" .= bkTarget b,
        "created-data" .= bkCreated b,
        "last-used" .= bkLastUsed b]

type BKMap = Map.Map Text Bookmark

maxBkByLabel :: Bookmark -> Bookmark -> Ordering
maxBkByLabel bk1 bk2 = (DT.length label1) `compare` (DT.length label2)
    where
        label1 = bkLabel bk1
        label2 = bkLabel bk2

maxOffsetBKMap :: BKMap -> Int
maxOffsetBKMap bkMap = 
    DT.length . bkLabel $ 
        maximumBy maxBkByLabel  bkMap

labelOffset :: Int -> Text -> Int
labelOffset maxOffset label = maxOffset - (DT.length label)

showBKAssignment :: Int -> BKType -> Text -> Text -> Text
showBKAssignment maxOffset bktype label target = 
       (showBKType bktype) <> DT.replicate (offsetBKType bktype) " "
    <> label <> DT.replicate (labelOffset maxOffset label) " " <> " = " <> (DT.show target)

showBKMap :: Int -> BKMap -> Text
showBKMap maxOffset bkMap = _showBKMap maxOffset $ Map.assocs bkMap

_showBKMap :: Int -> [(Text,Bookmark)] -> Text
_showBKMap _ [] = ""
_showBKMap maxOffset [(label,bk)] = showBKAssignment maxOffset (bkType bk) label (bkTarget bk)
_showBKMap maxOffset ((label,bk):bks) 
     = showBKAssignment maxOffset (bkType bk) label (bkTarget bk) <> "\n"
    <> _showBKMap maxOffset bks

_logDebug 
    :: String 
    -> Linear.RIO ()
_logDebug !s = Linear.do
    h <- Linear.unsafeAcquire (Linear.return (Linear.Ur ())) (\_ -> Linear.return ())
    r <- Linear.unsafeFromSystemIOResource_ (\_ -> putStrLn s) h
    Linear.release r

feedCSVFile 
    :: (BS.ByteString -> CsvInc.Parser Bookmark)
    -> Linear.Handle %1
    -> Linear.RIO (Linear.Ur (CsvInc.Parser Bookmark), Linear.Handle)
feedCSVFile parserFam csvFile = Linear.do
    (Linear.Ur isEOF, csvFile') <- Linear.hIsEOF csvFile
    if isEOF
    then Linear.return (Linear.Ur (parserFam BS.empty),csvFile')
    else Linear.do
        (Linear.Ur line,csvFile'') <- Linear.hGetLine csvFile'
        let line' = WBL.textUTF8ToByteString (Data.Text.concat [line,(Data.Text.pack "\n")])
        let parser = parserFam line'
        Linear.return (Linear.Ur parser,csvFile'')

updateAcc :: ([Text], BKMap)
          -> [Either String Bookmark]
          -> ([Text], BKMap)
updateAcc acc = Prelude.foldl upAcc acc
    where
        upAcc (errs,bkMap) (Left errMsg) = (errs <> [DT.pack errMsg],bkMap)
        upAcc (errs,bkMap) (Right b)     = (errs,Map.insert (bkLabel b) b bkMap)

loop :: ([Text], BKMap)
     -> Linear.Handle %1
     -> CsvInc.Parser Bookmark 
     -> Linear.RIO (Linear.Ur ([Text], BKMap))
loop acc csvFile (CsvInc.Fail _ errMsg)   
    = Linear.do 
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc [Left errMsg]))

loop acc csvFile (CsvInc.Many rs parserFam) 
    = Linear.do         
        (Linear.Ur parser,csvFile'') <- feedCSVFile parserFam csvFile
        loop (updateAcc acc rs) csvFile'' parser                                       

loop acc csvFile (CsvInc.Done rs) 
    = Linear.do     
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc rs))

readCSVFile 
    :: FilePath 
    -> IO ([Text], BKMap)
readCSVFile csvFilePath = Linear.run $ readCSVFile' 
    where
        readCSVFile' 
            :: Linear.RIO (Linear.Ur ([Text], BKMap))
        readCSVFile' = Linear.do            
            csvFile <- Linear.openFile csvFilePath ReadMode
            (Linear.Ur contents) <- loop ([],Map.empty) csvFile (decode HasHeader) 
            Linear.return (Linear.Ur contents)

writeCSVFile
    :: FilePath
    -> BKMap
    -> IO ()
writeCSVFile csvFilePath bkMap = Linear.run writeCSVFile' 
    where        
        bks = WBL.byteStringToTextUTF8 . 
                WBL.lazyByteStringToByteString $ 
                    encodeByName header $ Map.elems bkMap

        writeCSVFile' :: Linear.RIO (Linear.Ur ())
        writeCSVFile' = Linear.do
            csvFile <- Linear.openFile csvFilePath WriteMode
            csvFile' <- Linear.hPutStr csvFile bks
            Linear.release csvFile'
            Linear.return (Linear.Ur ())

addBookmark :: Bookmark
            -> BKMap
            -> BKMap
addBookmark b bkMap = Map.insert (bkLabel b) b bkMap

removeBookmark :: Text
               -> BKMap
               -> BKMap
removeBookmark label = Map.delete label

findBookmark :: Text
             -> BKMap
             -> Prelude.Maybe Bookmark
findBookmark label = Map.lookup label

partitionBKMap
    :: (Bookmark -> Bool)
    -> BKMap
    -> (BKMap,BKMap)
partitionBKMap pick = foldl which (Map.empty,Map.empty)
    where
        which :: (BKMap,BKMap) 
              -> Bookmark 
              -> (BKMap,BKMap)
        which (p1,p2) bk | pick bk   = (Map.insert (bkLabel bk) bk p1,p2)
        which (p1,p2) bk | otherwise = (p1,Map.insert (bkLabel bk) bk p2)

isAlias :: Bookmark -> Bool
isAlias (Bookmark BKAlias _ _ _ _) = True
isAlias _ = False

recentBookmarks :: Day
                -> BKMap
                -> (BKMap,BKMap)
recentBookmarks today bkMap = partitionBKMap isAlias $ Map.filter recentBookmark bkMap
    where                        
        recentBookmark :: Bookmark -> Bool
        recentBookmark (Bookmark _ _ _ _ lastUsedDay) = 
            addDays (-10) today <= lastUsedDay && lastUsedDay <= today

handler_ :: (BKMap  -> IO ()) -> IO ()
handler_ handle = _handler False (\m -> handle m >> return m)

handler :: (BKMap  -> IO BKMap) -> IO ()
handler = _handler True

initializeWorkDir :: IO FilePath
initializeWorkDir = do 
    homeDir <- WBL.getHomeDirectory
    let wdir = homeDir <> "/.bk"
    let bookmarkCSVFile = wdir <> "/bk-bookmarks.csv"
    WBL.createDirectoryIfMissing False wdir
    bookmarkCSVFileExists <- WBL.doesFileExist bookmarkCSVFile
    when (not bookmarkCSVFileExists) $ 
        writeCSVFile bookmarkCSVFile Map.empty
    return bookmarkCSVFile

-- Change (BKMap -> IO BKMap) to ((Int,BKMap) -> IO BKMap).
_handler :: Bool -> (BKMap  -> IO BKMap) -> IO ()
_handler writeMode action = 
    do bookmarkCSVFile <- initializeWorkDir     
       (errs,csvContents) <- readCSVFile bookmarkCSVFile       
       if null errs
       then do csvContents' <- action csvContents                                       
               when writeMode $ writeCSVFile bookmarkCSVFile csvContents'
               exitSuccess
       else print errs >> exitFailure
