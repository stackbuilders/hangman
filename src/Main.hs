module Main (main) where

import Hangman (playHangman)

import System.IO
import Test.QuickCheck (elements,generate)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  allHangmanWords defaultHangmanWordsFile >>= playHangmans Nothing

playHangmans :: Maybe Int -> [String] -> IO ()
playHangmans _ [] =
  putStrLn
    "Now I can do no more, whatever happens. What will become of me?"
playHangmans maybeLives hangmanWords = do
  hangmanWord <- randomHangmanWord hangmanWords
  playHangman maybeLives hangmanWord
  playAgain <- playHangmanAgain
  if playAgain
     then playHangmans maybeLives hangmanWords
     else putStrLn "He's murdering the time! Off with his head!"

playHangmanAgain :: IO Bool
playHangmanAgain = do
  putStrLn "Let's go on with the game... (yes or no)"
  yesOrNo <- getLine
  case yesOrNo of
    "yes" -> return True
    "no"  -> return False
    _     -> playHangmanAgain

defaultHangmanWordsFile :: String
defaultHangmanWordsFile = "words/words.txt"

allHangmanWords :: String -> IO [String]
allHangmanWords = fmap words . readFile

randomHangmanWord :: [String] -> IO String
randomHangmanWord = generate . elements
