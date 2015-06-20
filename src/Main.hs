{-# LANGUAGE RecordWildCards #-}

-- | TODO

module Main (main) where

import Hangman (playHangman)

import Data.Char (toLower)
import Data.Maybe (fromJust,fromMaybe)
import Control.Exception (tryJust)
import Control.Monad (when)
import System.Console.GetOpt
  (ArgDescr(NoArg,ReqArg)
  ,ArgOrder(Permute)
  ,getOpt
  ,OptDescr(Option)
  ,usageInfo)
import System.Directory (doesFileExist,getPermissions,readable)
import System.Environment (getArgs,getProgName)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering),stdout)
import System.IO.Error (isDoesNotExistError,isPermissionError)
import Test.QuickCheck (elements,generate)

----------------------------------------------------------------------

main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName

  Options{..} <- getOptions args

  hSetBuffering stdout NoBuffering

  putStrLn progName

  when optionHelp (putStrLn "help" >> exitSuccess)

  when optionVersion (putStrLn "version" >> exitSuccess)

  maybeWordsFile <- getWordsFile optionWordsFile

  case maybeWordsFile of
    Nothing -> undefined
    Just wordsFile -> do
      putStrLn ("words file... " ++ wordsFile)

      readWords wordsFile >>= play Nothing

----------------------------------------------------------------------

play :: Maybe Int -> [String] -> IO ()
play _ [] =
  putStrLn
    "Now I can do no more, whatever happens. What will become of me?"
play maybeLives hangmanWords = do
  hangmanWord <- fmap fromJust (getWord hangmanWords)
  playHangman maybeLives (fmap toLower hangmanWord)
  playAgain <- playHangmanAgain
  if playAgain
     then play maybeLives hangmanWords
     else putStrLn "He's murdering the time! Off with his head!"

playHangmanAgain :: IO Bool
playHangmanAgain = do
  putStrLn "Let's go on with the game... (yes or no)"
  yesOrNo <- getLine
  case yesOrNo of
    "yes" -> return True
    "no"  -> return False
    _     -> playHangmanAgain


defaultWordsFiles :: [FilePath]
defaultWordsFiles =
  [ "words/words.txt"
  , "/usr/share/dict/words"
  ]

getDefaultWordsFile :: IO (Maybe FilePath)
getDefaultWordsFile = do
  r <- sequence (fmap isFileReadable defaultWordsFiles)
  let sd = zip (fmap fst r) defaultWordsFiles
  return (lookup True sd)


getWordsFile :: Maybe FilePath -> IO (Maybe FilePath)
getWordsFile Nothing          = getDefaultWordsFile
getWordsFile (Just wordsFile) = do
  putStrLn ("checking... " ++ wordsFile)

  (isR,mer) <- isFileReadable wordsFile

  if isR
     then return (Just wordsFile)
     else do
       putStrLn (fromMaybe "er" mer)
       getWordsFile Nothing

isFileReadable :: FilePath -> IO (Bool, Maybe String)
isFileReadable file = do
  sd <- doesFileExist file
  if not sd
     then return (False, Just "does not exist or is a directory")
     else do
       sss <- tryJust han (fmap readable (getPermissions file))
       case sss of
         Left er -> return (False, Just er)
         Right False -> return (False, Just "not readable")
         Right True -> return (True, Nothing)
  where
    han :: IOError -> Maybe String
    han er
      | isDoesNotExistError er = Just "the file does not exist"
      | isPermissionError er = Just "no permissions"
      | otherwise = Nothing

readWords :: FilePath -> IO [String]
readWords = fmap words . readFile

getWord :: [String] -> IO (Maybe String)
getWord [] = return Nothing
getWord ws = fmap Just (generate (elements ws))


----------------------------------------------------------------------

data Options = Options
  { optionHelp      :: Bool
  , optionVersion   :: Bool
  , optionWordsFile :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optionHelp      = False
    , optionVersion   = False
    , optionWordsFile = Nothing
    }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions =
  [ Option
      "h"
      ["help"]
      (NoArg (\options -> options {optionHelp = True}))
      ""
  , Option
      "v"
      ["version"]
      (NoArg (\options -> options {optionVersion = True}))
      ""
  , Option
      "w"
      ["words"]
      (ReqArg (\arg options -> options {optionWordsFile = Just arg}) "")
      ""
  ]

getOptions :: [String] -> IO Options
getOptions args =
  case getOpt Permute optionDescriptions args of
    (options,[],[]) -> return (foldl (flip id) defaultOptions options)
    (_,_,ers) ->
     ioError (userError (concat ers ++ usageInfo header optionDescriptions))
  where
    header = "Usage: hangman [OPTION...]"
