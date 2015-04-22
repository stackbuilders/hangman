module Main (main) where

import Hangman

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hangmanWords "words/words.txt" >>= playHangman Nothing

hangmanWords :: String -> IO [String]
hangmanWords = fmap words . readFile
