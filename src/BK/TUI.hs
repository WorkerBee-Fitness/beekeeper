{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DataKinds              #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module BK.TUI (mainLoop) where

--
-- * External Imports:
--
import Data.Text          (Text)
import System.Exit        (ExitCode(..)
                          ,exitFailure
                          ,exitWith, exitSuccess)
import Data.Time.Calendar (Day)
import Data.Time          (getCurrentTime
                          ,UTCTime (..))
import Data.List          (uncons)
import System.Process     (readCreateProcessWithExitCode
                          ,shell)
import System.IO          (hPutStr
                          ,stdout
                          ,stderr)

import Options.Applicative  qualified as OptA
import Data.Attoparsec.Text qualified as Atto
import Data.Text            qualified as DT

--
-- * Internal Imports:                                     
-- 
import BK.BKMap  (BKType (..)
                 ,Bookmark (..)
                 ,BKMap
                 ,showBKType
                 ,addBookmark
                 ,bookmark
                 ,findBookmark
                 ,removeBookmark
                 ,maxOffsetBKMap
                 ,recentBookmarks
                 ,showBKMap
                 ,filterBKMap
                 ,isAlias
                 ,isBookmark
                 ,readCSVFile
                 ,writeCSVFile
                 ,initializeWorkDir
                 ,filterLabels
                 ,renameBookmark)

import BK.Lib    qualified as Lib
import Data.Char qualified as DT

_progName :: String
_progName = "bk"

_progVersion :: String
_progVersion = "v0.2"

--
-- * Option parsing
-- 

-- | All available command-line options.
data BKOption
    = OptAddBK BKType Text Text -- ^ Option "add" handles both bookmarks and aliases.
    | OptRenameBK Text Text     -- ^ Option "rename" handles renaming a bookmark/alias.
    | OptRunBK Text [Text]      -- ^ Option "run" handles running a bookmark with arguments.
    | OptRemoveBK Text          -- ^ Option "remove" handles removing a bookmark/alias.    
    | OptFindBK Text            -- ^ Option "find" searches for a bookmark.
    | OptList                   -- ^ Option "list" outputs all bookmarks and aliases to the terminal.
    | OptBookmarks              -- ^ Option "bookmarks" outputs all bookmarks to the terminal.    
    | OptAliases                -- ^ Option "aliases" outputs all aliases to the terminal.    
    | OptRecentsBK              -- ^ Option "recents" outputs the recently added bookmarks and aliases to the terminal.
    deriving (Show)

--
-- ** Attoparsec Parsers
-- 

-- | Parses an assignment of the form @label=target@.
-- Skips over whitespace before and after the @=@.
assignParser :: Atto.Parser (Text,Text)
assignParser = do 
    label <- labelParser
    Atto.skipSpace
    Atto.skip (=='=')
    Atto.skipSpace
    target <- targetParser
    return $ (label,target)

-- | Parses a bookmark label.
-- Labels must start with a letter and then be alpha-numeric with the addition of @_@ and @-@.
labelParser :: Atto.Parser Text
labelParser = do
    startChar <- Atto.take 1      
    if DT.all DT.isAlpha startChar
    then do
        rest <- Atto.takeWhile1 $ DT.isAlphaNum `Lib.orF` (`elem` ['-','_'])             
        return $ DT.append startChar rest
    else fail $ "parse error: bookmark labels must start with a letter."

-- | Parses the target of a bookmark.
-- Currently, there are no conditions on what a target can be, but in the future
-- we will need to modify this to handle variables. 
-- 
-- Reminder: the shell seems to be removing any quotes from the assignment.
targetParser :: Atto.Parser Text
targetParser = Atto.takeText            

-- | Parses the label of a bookmark.
labelArg :: OptA.ReadM Text
labelArg = OptA.eitherReader $ Atto.parseOnly labelParser . DT.pack

-- | Parses an "add" option (`OptAddBK`) from an assignment argument.
optAddBKParser 
    :: BKType -- ^ Type of the bookmark being added
    -> Atto.Parser BKOption
optAddBKParser bkType = do 
    (label,target) <- assignParser
    return $ OptAddBK bkType label target

--
-- ** Optparse-Applicative Parsers
-- 

-- | Imports the `optAddBKParser` into optparse-applicative as an argument
-- parser.
bkAddBkParser 
    :: BKType -- ^ Type of the bookmark being added
    -> OptA.Parser BKOption
bkAddBkParser bkType = OptA.argument (parser bkType) (OptA.metavar "LABEL=TARGET")
    where
        parser :: BKType -> OptA.ReadM BKOption
        parser bkType = OptA.eitherReader $ Atto.parseOnly (optAddBKParser bkType) . DT.pack

completeLabel :: BKMap -> String -> IO [String]
completeLabel bkMap partial = do
    putStrLn "Completer called"
    return . map DT.unpack $ filterLabels (\l -> (DT.pack partial) `DT.isPrefixOf` l) bkMap

-- | Parses the arguments to the run option.
bkRunCmdParser 
    :: BKMap
    -> OptA.Parser BKOption
bkRunCmdParser bkMap = OptRunBK
                    <$> OptA.argument labelArg (OptA.metavar "LABEL" <> OptA.completer (OptA.mkCompleter (completeLabel bkMap)))  
                    <*> (OptA.many $ OptA.argument OptA.str (OptA.metavar "ARGS"))    

-- | Parses the arguments to the @remove@ command.
bkRemoveCmdParser 
    :: BKMap
    -> OptA.Parser BKOption    
bkRemoveCmdParser bkMap 
    = OptRemoveBK
   <$> OptA.argument labelArg (OptA.metavar "LABEL" <> OptA.completer (OptA.mkCompleter (completeLabel bkMap)))      

-- | Parses the arguments to the @rename@ command.
bkRenameCmdParser 
    :: BKMap
    -> OptA.Parser BKOption    
bkRenameCmdParser bkMap 
    = OptRenameBK
   <$> OptA.argument labelArg (OptA.metavar "LABEL1" <> OptA.completer (OptA.mkCompleter (completeLabel bkMap)))      
   <*> OptA.argument labelArg (OptA.metavar "LABEL2")      

-- | Parses the argument to @find@ command.
bkFindCmdParser :: OptA.Parser BKOption
bkFindCmdParser = OptFindBK
               <$> OptA.argument labelArg (OptA.metavar "LABEL")      

-- | Parses the @list@ command.
bkListCmdParser :: OptA.Parser BKOption
bkListCmdParser = pure OptList

-- | Parses the @bookmarks@ command.
bkListBksCmdParser :: OptA.Parser BKOption
bkListBksCmdParser = pure OptBookmarks

-- | Parses the @aliases@ command.
bkListAliasesCmdParser :: OptA.Parser BKOption
bkListAliasesCmdParser = pure OptAliases

-- | Parses the @aliases@ command.
bkRecentsParser :: OptA.Parser BKOption
bkRecentsParser = pure OptRecentsBK

-- | Parses the various command-line options.
bkCmdParser :: BKMap -> OptA.Parser BKOption
bkCmdParser bkMap = OptA.hsubparser 
    (  OptA.command "bookmark"  (OptA.info (bkAddBkParser BKBookmark) (OptA.progDesc "Add a bookmark"))
    <> OptA.command "alias"     (OptA.info (bkAddBkParser BKAlias)    (OptA.progDesc "Add an alias"))
    <> OptA.command "run"       (OptA.info (bkRunCmdParser bkMap)           (OptA.progDesc "Runs a bookmark or alias"))
    <> OptA.command "rename"    (OptA.info (bkRenameCmdParser bkMap)        (OptA.progDesc "Renames a bookmark or alias"))
    <> OptA.command "remove"    (OptA.info (bkRemoveCmdParser bkMap)        (OptA.progDesc "Removes a bookmark or alias"))
    <> OptA.command "find"      (OptA.info (bkFindCmdParser)          (OptA.progDesc "Searches for a bookmark or alias"))
    <> OptA.command "list"      (OptA.info (bkListCmdParser)          (OptA.progDesc "Lists all bookmarks and aliases"))
    <> OptA.command "bookmarks" (OptA.info (bkListBksCmdParser)       (OptA.progDesc "Lists all bookmarks"))
    <> OptA.command "aliases"   (OptA.info (bkListAliasesCmdParser)   (OptA.progDesc "Lists all aliases"))
    )
    OptA.<|> (bkRunCmdParser bkMap)  -- run is the default when no commands are given.
    OptA.<|> bkRecentsParser         -- No options or arguments, then show recents.

-- | Generates the various additional options and messages using the command
-- parser. This adds the @--help@ and @--version@ options as well as the program
-- description and header for the help message.
bkOpts 
    :: BKMap                     -- Contents of CSV file
    -> OptA.ParserInfo BKOption
bkOpts bkMap = OptA.info ((bkCmdParser bkMap) OptA.<**> OptA.helper OptA.<**> OptA.simpleVersioner _progVersion)
    (OptA.fullDesc <> OptA.progDesc _progName <> OptA.header "BeeKeeper remembers so you don't have to!")

--
-- * Option Handlers
-- 

-- | Runs an options handler.
handleOpt 
    :: BKOption -- ^ Option to handle
    -> BKMap
    -> IO (Maybe BKMap)
handleOpt OptRecentsBK        bkMap = handleRecentsbk bkMap
handleOpt OptList             bkMap = handleListBookmarks Nothing bkMap
handleOpt OptBookmarks        bkMap = handleListBookmarks (Just BKBookmark) bkMap
handleOpt OptAliases          bkMap = handleListBookmarks (Just BKAlias) bkMap
handleOpt (OptAddBK ty l t)   bkMap = handleAddbk ty l t bkMap
handleOpt (OptFindBK l)       bkMap = handleFindbk l bkMap
handleOpt (OptRunBK l args)   bkMap = handleRunbk l args bkMap
handleOpt (OptRemoveBK l)     bkMap = handleRemovebk l bkMap
handleOpt (OptRenameBK l1 l2) bkMap = handleRenamebk l1 l2 bkMap

-- | Handler for the @add@ option.
handleAddbk 
    :: BKType -- ^ Type of the bookmark being added
    -> Text   -- ^ Label of the bookmark being added
    -> Text   -- ^ Target of the bookmark being added
    -> BKMap  -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleAddbk ty l t csvContents = do
    createdDay <- today
    Just <$> handleAddbk' ty l t createdDay csvContents
    where
        today :: IO Day
        today = do
            currentUTCTime <- getCurrentTime
            return . utctDay $ currentUTCTime

        handleAddbk' :: BKType -> Text -> Text -> Day -> BKMap -> IO BKMap
        handleAddbk' typebk labelbk targetbk createdbk csvContents
            = either
                (\errMsg -> Lib.putStrLnStdErr ("error: "<>errMsg) >> return csvContents)
                (\b -> do homedir <- Lib.getHomeDirectory
                          either 
                              (\errMsg -> do Lib.putStrLnStdErr $ "error: " <> errMsg
                                             exitFailure)
                              (\updatedMap -> 
                                  do putStrLn $ "created " <> DT.unpack (showBKType typebk)  <> " \"" <> (DT.unpack labelbk) <> "\""
                                     return updatedMap)
                              $ addBookmark b homedir csvContents)
              $ bookmark typebk labelbk targetbk createdbk createdbk 

-- | Handler for the @find@ option.
handleFindbk 
    :: Text  -- ^ Label of the bookmark to search for
    -> BKMap -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleFindbk labelbk csvContents = do
    maybe (Lib.putStrLnStdErr $ "bookmark not found " <> DT.show labelbk)
          print
        $ findBookmark labelbk csvContents
    return Nothing

-- | Handler for the @remove@ option.
handleRemovebk 
    :: Text  -- ^ Label of the bookmark to remove
    -> BKMap -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleRemovebk labelbk csvContents = do
        putStrLn $ "removed bookmark " <> show (DT.unpack labelbk)
        return . Just $ removeBookmark labelbk csvContents

-- | Handler for the @rename@ option.
handleRenamebk 
    :: Text  -- ^ Label of the bookmark to rename
    -> Text  -- ^ new label
    -> BKMap -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleRenamebk old_labelbk new_labelbk bkMap = do
        let mNewMap = renameBookmark old_labelbk new_labelbk bkMap
        case mNewMap of
            Just newMap -> do putStrLn . DT.unpack $ "renamed " <> Lib.dq_text old_labelbk <> " to " <> Lib.dq_text new_labelbk
                              pure . Just $ newMap
            Nothing -> do Lib.putStrLnStdErr $ "error: bookmark " <> Lib.dq_text old_labelbk <> " couldn't be found."
                          pure Nothing        

-- | Handler for the @run@ option.
handleRunbk 
    :: Text   -- ^ Label of the bookmark to run
    -> [Text] -- ^ List of arguments to pass to the target
    -> BKMap  -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleRunbk labelbk inArgs csvContents = do
    case findBookmark labelbk csvContents of
        Nothing -> Lib.putStrLnStdErr $ "bookmark not found " <> DT.show labelbk
        Just b -> 
            case bkType b of
                BKBookmark -> putStrLn . DT.unpack . bkTarget $ b
                BKAlias -> do
                    let cmdM = uncons $ DT.words $ bkTarget b
                    maybe 
                        (Lib.putStrLnStdErr $ "error: target is empty for label \"" <> (bkLabel b) <> "\"")
                        (\(cmd',savedArgs) -> do let args = savedArgs <> inArgs
                                                 let cmd = DT.unpack . DT.unwords $ cmd':args
                                                 putStrLn $ "running \""<>cmd<>"\""                                                 
                                                 (exCode,out,err) <- readCreateProcessWithExitCode (shell cmd) ""
                                                 case exCode of
                                                    ExitSuccess -> do putStr out                                                                       
                                                                      exitWith ExitSuccess
                                                    ExitFailure i -> do hPutStr stdout out
                                                                        hPutStr stderr err
                                                                        exitWith $ ExitFailure i)
                        cmdM
    return Nothing

-- | Handler for the @recents@ option.
handleRecentsbk 
    :: BKMap            -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleRecentsbk csvContents = 
        do today <- getCurrentTime
           let maxLabelOffset = maxOffsetBKMap csvContents
           let (recAliases,recBks) = recentBookmarks (utctDay today) csvContents
           --TODO: Need to pull this out into a function def:
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recAliases
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recBks
           return Nothing

-- | Handler for the @list@ option.
-- If the input is @Nothing@ then list all of the bookmarks and aliases, but if
-- it's @Just bkType@ then filter the list based on the value of @bkType@.
handleListBookmarks 
    :: Maybe BKType -- ^ List filter
    -> BKMap        -- ^ Contents of the CSV file
    -> IO (Maybe BKMap)
handleListBookmarks mbkType csvContents = 
    do let _map = filterBKMap (pred mbkType) csvContents
       let maxLabelOffset = maxOffsetBKMap _map
       putStrLn . DT.unpack  $ showBKMap maxLabelOffset _map
       return Nothing
    where
        pred :: Maybe BKType -> Bookmark -> Bool
        pred Nothing _ = True
        pred (Just BKAlias) bk    = isAlias bk
        pred (Just BKBookmark) bk = isBookmark bk

-- | The main loop. 
-- This is called by the `Main` module.
mainLoop ::  IO ()
mainLoop = do
    bookmarkCSVFile <- initializeWorkDir     
    (errs,csvContents) <- readCSVFile bookmarkCSVFile       
    if null errs
    then OptA.execParser (bkOpts csvContents) >>= \opt ->
            do updatedMapM <- handleOpt opt csvContents
               maybe (return ())
                     (writeCSVFile bookmarkCSVFile)
                     updatedMapM
               exitSuccess
    else print errs >> exitFailure
