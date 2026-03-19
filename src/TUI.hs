{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module TUI
    (mainLoop) where

import Data.Data 
    (Typeable, 
     Data)

import System.Console.CmdArgs
    (Default (def),
     (&=),
     summary,
     help,
     cmdArgsMode,
     cmdArgsRun,
     args,
     modes, auto, Mode)
import System.Environment (withArgs)
import Control.Monad (void)
import System.Console.CmdArgs (CmdArgs)
import BK (parseBKType, addBookmark, Bookmark (..), removeBookmark, findBookmark, handler, handler_)
import qualified WBeeLib.ByteString as WBL
import Data.Text (pack, Text)
import System.Exit (exitFailure)
import qualified Data.ByteString as BS

data BKMode 
    = Default {
            find    :: Maybe String,
            remove  :: Maybe String,
            run     :: Maybe String
        } 
    | Add {
            kind   :: Maybe String,
            label  :: Maybe String,
            target :: Maybe String
        }
    deriving (Show, Data, Typeable)

data AddOption = AddOption  deriving (Show, Data, Typeable)

addOption :: BKMode
addOption = Add {
    kind = def &= help "the type of bookmark",
    label = def &= help "the label to add",
    target = def &= help "the command to add"
}

option :: BKMode
option = Default {       
    find   = def &= help "find a bookmark",
    remove = def &= help "remove a bookmark",
    run    = def &= args
} &= auto

mainModes :: Mode  (CmdArgs BKMode)
mainModes = cmdArgsMode $ modes [option, addOption] 
    &= summary "bk 0.0.0.1"

handleAddbk :: String -> String -> String -> IO ()
handleAddbk (WBL.stringUTF8ToByteString->ty) (pack->l) (pack->t)= handleAddbk' ty l t
    where
        handleAddbk' :: BS.ByteString -> Text -> Text -> IO ()
        handleAddbk' (parseBKType->Right typebk) labelbk targetbk 
            = let b = Bookmark { bkType = typebk, bkLabel = labelbk, bkTarget = targetbk } 
               in handler (return . addBookmark b)
        handleAddbk' (parseBKType->Left err) _ _ = print err >> exitFailure

handleFindbk :: String -> IO ()
handleFindbk (pack->labelbk) = handler_
    (\csvContents -> case findBookmark labelbk csvContents of
                        Nothing -> putStrLn $ "bookmark not found " ++ (show labelbk)
                        Just b ->  print b)

handleRemovebk :: String -> IO ()
handleRemovebk (pack->labelbk) 
    = handler (return . removeBookmark labelbk)

handleRunbk :: String -> IO ()
handleRunbk _labelbk = undefined

mainLoop ::  IO ()
mainLoop = do
    opts <- cmdArgsRun mainModes 
    case opts of
        Add Nothing _ _ -> error "error: add: [--kind] required"
        Add _ Nothing _ -> error "error: add: [--type] required"
        Add _ _ Nothing -> error "error: add: [--cmd] required"
        Add (Just t) (Just l) (Just c) -> handleAddbk t l c
        Default (Just l) _ _  -> handleFindbk l
        Default _ (Just l) _  -> handleRemovebk l
        Default _ _ (Just l)  -> handleRunbk l
        Default _ _ Nothing   -> 
            void $ withArgs ["--help"] $ cmdArgsRun mainModes
