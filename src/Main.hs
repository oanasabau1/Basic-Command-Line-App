{-# LANGUAGE BlockArguments #-}
module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  database <- DB.save DB.empty  -- save a empty database to init, it doesn't require displaying an error message 
  return()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  database <- DB.load  --load the database
  case database of  --check if exists and display messages accordingly
    Error err -> putStrLn "Failed to load DB"
    Success db ->
      let
        entryFound = DB.findFirst (\entry -> entryId entry == getOptId getOpts) db
      in
        case entryFound of
          Just entry -> putStrLn (entrySnippet entry)
          Nothing -> putStrLn "Unable to find entry by given ID"


-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  database <- DB.load  --load the database
  case database of  --check if exists and display messages accordingly
    Error err -> putStrLn "Failed to load DB"
    Success db ->
      let
        entriesFound = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db
      in
        case entriesFound of
          [] -> putStrLn "No entries found"
          _ -> putStrLn (foldl (++) "" (map (\entry -> show (FmtEntry entry) ++ "\n") entriesFound)) --display all entries that match all search queries


-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  database <- DB.load  --load the database
  snippet <- readFile (addOptFilename addOpts) -- read the contents of the file
  case database of  --check if exists and display messages accordingly
    Error err -> putStrLn "Failed to load DB"
    Success db ->
      let
        entryChecked = DB.findFirst (\entry -> entrySnippet entry == snippet) db --check for the entry 
      in
        case entryChecked of --check if the entry with the snippet to be added already exists
          Just entryFound -> 
            putStrLn ("Entry with this content already exists: \n" ++ show (FmtEntry entryFound))
          Nothing -> do
            DB.modify (DB.insertWith (\id -> makeEntry id snippet addOpts))
            return ()

  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
